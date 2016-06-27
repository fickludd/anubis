package se.lth.immun.anubis

import se.lth.immun.chem.Peptide
import se.lth.immun.unimod.UniMod
import se.lth.immun.xml.XmlReader
import se.lth.immun.xml.XmlWriter
import se.lth.immun.xml.XmlUtil
import java.io.File
import java.util.Calendar
import java.util.Date
import java.util.Locale
import java.text.DateFormat
import uk.ac.liv.pgb.jmzqml.model.mzqml.AnalysisSummary
import uk.ac.liv.pgb.jmzqml.model.mzqml.Assay
import uk.ac.liv.pgb.jmzqml.model.mzqml.AssayList
import uk.ac.liv.pgb.jmzqml.model.mzqml.Column
import uk.ac.liv.pgb.jmzqml.model.mzqml.ColumnDefinition
import uk.ac.liv.pgb.jmzqml.model.mzqml.Cv
import uk.ac.liv.pgb.jmzqml.model.mzqml.CvList
import uk.ac.liv.pgb.jmzqml.model.mzqml.CvParam
import uk.ac.liv.pgb.jmzqml.model.mzqml.CvParamRef
import uk.ac.liv.pgb.jmzqml.model.mzqml.DataMatrix
import uk.ac.liv.pgb.jmzqml.model.mzqml.DataProcessing
import uk.ac.liv.pgb.jmzqml.model.mzqml.DataProcessingList
import uk.ac.liv.pgb.jmzqml.model.mzqml.EvidenceRef
import uk.ac.liv.pgb.jmzqml.model.mzqml.Feature
import uk.ac.liv.pgb.jmzqml.model.mzqml.FeatureList
import uk.ac.liv.pgb.jmzqml.model.mzqml.FileFormat
import uk.ac.liv.pgb.jmzqml.model.mzqml.GlobalQuantLayer
import uk.ac.liv.pgb.jmzqml.model.mzqml.IdOnly
import uk.ac.liv.pgb.jmzqml.model.mzqml.InputFiles
import uk.ac.liv.pgb.jmzqml.model.mzqml.Label
import uk.ac.liv.pgb.jmzqml.model.mzqml.MethodFile
import uk.ac.liv.pgb.jmzqml.model.mzqml.MethodFiles
import uk.ac.liv.pgb.jmzqml.model.mzqml.ModParam
import uk.ac.liv.pgb.jmzqml.model.mzqml.MzQuantML
import uk.ac.liv.pgb.jmzqml.model.mzqml.PeptideConsensus
import uk.ac.liv.pgb.jmzqml.model.mzqml.PeptideConsensusList
import uk.ac.liv.pgb.jmzqml.model.mzqml.ProcessingMethod
import uk.ac.liv.pgb.jmzqml.model.mzqml.QuantLayer
import uk.ac.liv.pgb.jmzqml.model.mzqml.RawFile
import uk.ac.liv.pgb.jmzqml.model.mzqml.RawFilesGroup
import uk.ac.liv.pgb.jmzqml.model.mzqml.Row
import uk.ac.liv.pgb.jmzqml.model.mzqml.Software
import uk.ac.liv.pgb.jmzqml.model.mzqml.SoftwareList
import uk.ac.liv.pgb.jmzqml.xml.io.MzQuantMLMarshaller
import uk.ac.liv.pgb.jmzqml.model.mzqml.UserParam

object ResultFile {
	val MAIN_NODE = "results"
    val PARAMS = "parameters"
    val ANUBIS = "anubis"
    val ANUBIS_VERSION = "version"
    val WORKING_DIR = "working_dir"
    val REFERENCE_FILE = "reference_file"
    val PEAK_MIN_WIDTH = "peak_min_width"
    val NULL_DIST_SIZE = "null_distribution_size"
    val TRANS_LIMIT = "num_transition_limit"
    val FILES = "replicate_files"
    val FILE = "replicate_file"
    val RUN_START_TIME = "run_start_time"
    val PRECURSORS = "precursors"
    val PRECURSOR = "precursor"
    
    val REPLICATES = "replicates"
    	
    val AREAS = "transision_areas"
    val REPLICATE = "replicate"
    
    val TRANSITION = "transition"
    
    val QUALITY = "quality"
    val FDR = "fdr" //DEPRECATED, use P_VALUE instead
    val P_VALUE = "p_value"
    val NBR_CONDITIONS = "nbr_conditions"
    val PEAK_BEFORE_WINDOW_FLAG = "peakBeforeWindowFlag"
    val PEAK_AFTER_WINDOW_FLAG = "peakAfterWindowFlag"
   	val SINGLE_ANSWER = "singleAnswer"
    val P_VALUE_TOLERANCE = "pValueTolerance"
    	
	val MZ = "mz"
    val AREA = "area"
    val ION = "ion"
    val PEPTIDE_SEQ = "peptide_sequence"
    val PROTEIN = "protein_name"
    	
    val RT = "retention_time"
    val START = "start"
    val END = "end"
    val PEAK = "peak"
    	
    val RATIO = "transition_ratio"
    val ID1 = "id1"
    val ID2 = "id2"
    val MEAN = "mean"
    val STD_DEV = "std_dev"
}


class ResultFile(
		var parameters:Array[ResultParameter] = Array(),
	    var replicateFiles:Array[ResultMzMLFile] = Array(),
	    var precursors:Array[ResultPrecursor] = Array(),
	    r:XmlReader = null
) {
	def this(r:XmlReader) = this(null, null, null, r)
    	import ResultFile._
	
    
    var peakMinWidth:Double 		= 0.0
    var nullDistributionSize:Int 	= 0
    var transitionLimit:Int 		= 0
    var singleAnswer:Boolean 		= true
    var pValueTolerance:Double 		= 0.01
    var anubisVersion:String 		= null
    var referenceFile:File 			= null
    var workingDir:File 			= null
    
	if (r != null) {
	    r.until(MAIN_NODE)
	
	    r.ensure(PARAMS)
	    parameters = new Array(r.readAttributeInt(XmlUtil.COUNT))
	    for (i <- 0 until parameters.length) {
	        r.ensure(XmlUtil.PARAM)
	        parameters(i) = new ResultParameter(r)
	    }
	    
	    if (r.is(ANUBIS)) {
	    	if (r.hasAttribute(ANUBIS_VERSION))
	    		anubisVersion = r.readAttribute(ANUBIS_VERSION)
	    	if (r.is(WORKING_DIR)) {
	    		var fileStr = r.readAttributeString(XmlUtil.ABS_PATH)
		        if (File.separator != '\\')
		        	fileStr = fileStr.map(c => if (c == '\\') '/' else c)
			    workingDir = new File("", fileStr)
			}
	    	if (r.is(REFERENCE_FILE)) {
	    		var fileStr = r.readAttributeString(XmlUtil.ABS_PATH)
		        if (File.separator != '\\')
		        	fileStr = fileStr.map(c => if (c == '\\') '/' else c)
			    referenceFile = new File("", fileStr)
			}
	    	if (r.is(PEAK_MIN_WIDTH))
	    		peakMinWidth = r.readAttributeDouble(XmlUtil.VALUE)
	    	if (r.is(NULL_DIST_SIZE))
	    		nullDistributionSize = r.readAttributeInt(XmlUtil.VALUE)
	    	if (r.is(TRANS_LIMIT))
	    		transitionLimit = r.readAttributeInt(XmlUtil.VALUE)
	    	if (r.is(SINGLE_ANSWER))
	    		singleAnswer = r.readAttribute(XmlUtil.VALUE).toBoolean
	    	if (r.is(P_VALUE_TOLERANCE))
	    		pValueTolerance = r.readAttributeDouble(XmlUtil.VALUE)
	    }
	    
	    r.ensure(FILES)
	    replicateFiles = new Array(r.readAttributeInt(XmlUtil.COUNT))
	    for (i <- 0 until replicateFiles.length) {
	        r.ensure(FILE)
	        replicateFiles(r.readAttributeInt(XmlUtil.FILE_ID)) =
	            new ResultMzMLFile(r)
	    }
	
	    r.ensure(PRECURSORS)
	    precursors = new Array(r.readAttributeInt(XmlUtil.COUNT))
	    for (i <- 0 until precursors.length) {
	    	r.ensure(PRECURSOR)
	        precursors(i) = new ResultPrecursor(r)
	    }
	}

  def writeMzq(mzqFileName: String) = {
    var mzq = new MzQuantML
    mzq.setVersion("1.0.1")
    var now = Calendar.getInstance()
    mzq.setCreationDate(now);
    mzq.setId("anubis-" + now.getTimeInMillis())

    var cvs = new CvList()
    var cvList = cvs.getCv()
    // psi-ms
    var cv = new Cv()
    cv.setId("PSI-MS")
    cv.setUri("http://psidev.cvs.sourceforge.net/viewvc/*checkout*/psidev/psi/psi-ms/mzML/controlledVocabulary/psi-ms.obo")
    cv.setFullName("Proteomics Standards Initiative Mass Spectrometry Vocabularies")
    cv.setVersion("3.58.0")
    cvList.add(cv)

    //unimod
    var cv_unimod = new Cv()
    cv_unimod.setId("UNIMOD")
    cv_unimod.setUri("http://www.unimod.org/obo/unimod.obo")
    cv_unimod.setFullName("Unimod")
    cvList.add(cv_unimod)

    //unit
    var cv_uo = new Cv()
    cv_uo.setId("UO")
    cv_uo.setUri("http://obo.cvs.sourceforge.net/*checkout*/obo/obo/ontology/phenotype/unit.obo")
    cv_uo.setFullName("Unit Ontology")
    cvList.add(cv_uo)

    mzq.setCvList(cvs)

    var analysisSummary = new AnalysisSummary()
    analysisSummary.getParamGroup().add(createCvParam("SRM quantitation analysis", cv, "MS:1001838"))
    var cvParam = createCvParam("SRM feature level quantitation", cv, "MS:1002281")
    cvParam.setValue("true")
    analysisSummary.getParamGroup().add(cvParam)
    cvParam = createCvParam("SRM peptide level quantitation", cv, "MS:1002282")
    cvParam.setValue("true")
    analysisSummary.getParamGroup().add(cvParam)
    cvParam = createCvParam("SRM protein level quantitation", cv, "MS:1002283")
    cvParam.setValue("false")
    analysisSummary.getParamGroup().add(cvParam)
    cvParam = createCvParam("SRM proteingroup level quantitation", cv, "MS:1002284")
    cvParam.setValue("false")
    analysisSummary.getParamGroup().add(cvParam)
    mzq.setAnalysisSummary(analysisSummary);

    var inputFiles = new InputFiles()
    var rawFilesGroupList = inputFiles.getRawFilesGroup()
    var assays = new AssayList()
    assays.setId("AssayList_1")
    var assayList = assays.getAssay()
    var samples = replicateFiles.length
    for (i <- 0 until samples) {
      replicateFiles(i).writeMzq(i, cv, rawFilesGroupList, assayList)
    }
    mzq.setAssayList(assays)

    var methodFile = new MethodFile()
    var methodFiles = new MethodFiles()
    methodFile.setName(referenceFile.getName())
    methodFile.setLocation(referenceFile.getAbsolutePath())
    methodFile.setId("ref")
    var format = new FileFormat()
    cvParam = createCvParam("TraML format", cv, "MS:1002411");
    format.setCvParam(cvParam)
    methodFile.setFileFormat(format)
    methodFiles.getMethodFile().add(methodFile)
    inputFiles.setMethodFiles(methodFiles)
    mzq.setInputFiles(inputFiles)

    var softwareList = new SoftwareList()
    var software = new Software()
    softwareList.getSoftware().add(software)
    software.setId("Anubis")
    software.setVersion(anubisVersion)
    software.getCvParam().add(createCvParam("Anubis", cv, "MS:1002410"))
    mzq.setSoftwareList(softwareList)

    var dataProcessingList = new DataProcessingList()
    var dataProcessing = new DataProcessing()
    dataProcessing.setId("transition_quantification")
    dataProcessing.setSoftware(software)
    dataProcessing.setOrder(java.math.BigInteger.ONE)
    var processingMethod = new ProcessingMethod()
    processingMethod.setOrder(java.math.BigInteger.ONE)
    processingMethod.getParamGroup().add(createCvParam("quantification data processing", cv, "MS:1001861"))
    // No filtering is done in the mzq output file.
    // processingMethod.getParamGroup().add(createUserParam(P_VALUE_TOLERANCE,pValueTolerance.toString()))
    processingMethod.getParamGroup().add(createUserParam(PEAK_MIN_WIDTH,peakMinWidth.toString()))
    processingMethod.getParamGroup().add(createUserParam(NULL_DIST_SIZE,nullDistributionSize.toString()))
    processingMethod.getParamGroup().add(createUserParam(TRANS_LIMIT,transitionLimit.toString()))
    dataProcessing.getProcessingMethod().add(processingMethod)
    dataProcessingList.getDataProcessing().add(dataProcessing)
    mzq.setDataProcessingList(dataProcessingList);

    var peptideConsensusListTop = mzq.getPeptideConsensusList()
    var featureListTop = mzq.getFeatureList();
    var featureLists: Array[FeatureList] = new Array(samples)
    var features: Array[java.util.List[Feature]] = new Array(samples)
    var transDataMatrix: Array[DataMatrix] = new Array(samples)
    for (i <- 0 until samples) {
      featureLists(i) = new FeatureList()
      features(i) = featureLists(i).getFeature()
      featureLists(i).setId("fl_" + i)
      featureLists(i).setRawFilesGroup(rawFilesGroupList.get(i))
      featureListTop.add(featureLists(i))

      var fql = featureLists(i).getFeatureQuantLayer()
      var cpRefArea = new CvParamRef()
      //cpRefArea.setCvParam(createCvParam("Q3 area", cv, "MS:100XXXX"))
      cpRefArea.setCvParam(createCvParam("XIC area", cv, "MS:1001858"))
      var featureQuantLayer = new GlobalQuantLayer();
      fql.add(featureQuantLayer);
      featureQuantLayer.setId("transition_areas_" + i);
      var featureColumnIndex = new ColumnDefinition()
      featureQuantLayer.setColumnDefinition(featureColumnIndex)
      var columnArea = new Column();
      columnArea.setIndex(java.math.BigInteger.ZERO);
      columnArea.setDataType(cpRefArea);
      featureColumnIndex.getColumn().add(columnArea)
      transDataMatrix(i) = new DataMatrix()
      featureQuantLayer.setDataMatrix(transDataMatrix(i))
    }
    var peptideConsensusList = new PeptideConsensusList;
    peptideConsensusList.setId("peplist");
    peptideConsensusList.setFinalResult(true);
    var peptideList = peptideConsensusList.getPeptideConsensus();
    var assayQuantLayer = peptideConsensusList.getAssayQuantLayer()
    var quantLayer: QuantLayer[IdOnly] = new QuantLayer()
    var quantQualLayer: QuantLayer[IdOnly] = new QuantLayer()
    quantLayer.setId("pep_quant")
    quantQualLayer.setId("pep_quality")
    var cpPValue = new CvParamRef()
    var cpArea = new CvParamRef()
    //cpArea.setCvParam(createCvParam("Total Q3 area", cv, "MS:100XXXX"))
    cpArea.setCvParam(createCvParam("Total XIC area", cv, "MS:1002412"))
    cpPValue.setCvParam(createCvParam("p-value", cv, "MS:1002072"))
    quantQualLayer.setDataType(cpPValue)
    quantLayer.setDataType(cpArea)
    var dataMatrix = new DataMatrix()
    var qualDataMatrix = new DataMatrix()
    quantLayer.setDataMatrix(dataMatrix)
    quantQualLayer.setDataMatrix(qualDataMatrix)
    for (assay <- 0 until assayList.size()) {
      quantLayer.getColumnIndex().add(assayList.get(assay).getId())
      quantQualLayer.getColumnIndex().add(assayList.get(assay).getId())
    }
    assayQuantLayer.add(quantLayer)
    assayQuantLayer.add(quantQualLayer)
    for (i <- 0 until precursors.length)
      peptideList.add(precursors(i).writeMzq(i, cv, cv_uo, assayList, features, dataMatrix, qualDataMatrix, transDataMatrix));
    peptideConsensusListTop.add(peptideConsensusList)

    var marshaller = new MzQuantMLMarshaller(mzqFileName)
    marshaller.marshall(mzq)
  }

    def write(w:XmlWriter) = {
    	
        w.startDocument
        w.startElement(MAIN_NODE)

        w.startListElement(PARAMS, parameters)
        for (p <- parameters)
            p.write(w)
        w.endElement
        
        w.startElement(ANUBIS)
        if (anubisVersion != null)
        	w.writeAttribute(ANUBIS_VERSION, anubisVersion)
        if (workingDir != null) {
        	w.startElement(WORKING_DIR)
        	w.writeAttribute(XmlUtil.ABS_PATH, workingDir.getCanonicalPath)
        	w.endElement
        }
        if (referenceFile != null) {
        	w.startElement(REFERENCE_FILE)
        	w.writeAttribute(XmlUtil.ABS_PATH, referenceFile.getCanonicalPath)
        	w.endElement
        }
        if (peakMinWidth > 0.0) {
        	w.startElement(PEAK_MIN_WIDTH)
        	w.writeAttribute(XmlUtil.VALUE, peakMinWidth)
        	w.endElement
        }
        if (nullDistributionSize > 0) {
        	w.startElement(NULL_DIST_SIZE)
        	w.writeAttribute(XmlUtil.VALUE, nullDistributionSize)
        	w.endElement
        }
        if (transitionLimit > 0) {
        	w.startElement(TRANS_LIMIT)
        	w.writeAttribute(XmlUtil.VALUE, transitionLimit)
        	w.endElement
        }
       	w.startElement(SINGLE_ANSWER)
       	w.writeAttribute(XmlUtil.VALUE, singleAnswer)
       	w.endElement
       	w.startElement(P_VALUE_TOLERANCE)
       	w.writeAttribute(XmlUtil.VALUE, pValueTolerance)
       	w.endElement
        w.endElement

        w.startListElement(FILES, replicateFiles)
        for (i <- 0 until replicateFiles.length) {
            replicateFiles(i).write(w, i)
        }
        w.endElement

        w.startListElement(PRECURSORS, precursors)
        for (pc <- precursors)
            pc.write(w)
        w.endElement

        w.endElement
        w.endDocument
    }
    
    def createCvParam(name: String, cv: Cv, accession: String): CvParam = {
	  var cp = new CvParam()
	  cp.setName(name)
	  cp.setCv(cv)
	  cp.setAccession(accession)
	  return cp
  	}
    
    def createUserParam(name: String, value: String): UserParam = {
      var up = new UserParam();
      up.setName(name)
      up.setValue(value)
      return up
    }
}



object ResultMzMLFile {
	val dateFormat = DateFormat.getDateTimeInstance(
							DateFormat.SHORT, 
							DateFormat.MEDIUM, 
							Locale.UK
						)
}

class ResultMzMLFile(
		var file:File,
		var runStartTime:Date, 
		r:XmlReader = null
) {
	def this(r:XmlReader) = this(null, null, r)
	import ResultFile._
	
	if (r != null) {
        var fileStr = r.readAttributeString(XmlUtil.ABS_PATH)
        if (File.separator != '\\')
        	fileStr = fileStr.map(c => if (c == '\\') '/' else c)
	    file = new File("", fileStr)
        if (r.hasAttribute(RUN_START_TIME))
	        runStartTime = 
	        	ResultMzMLFile.dateFormat.parse(
	        		r.readAttributeString(RUN_START_TIME)
	        	)
	    else
	    	runStartTime = new Date(0)
	}

    def write(w:XmlWriter, id:Int) {
       w.startElement(FILE)
       w.writeAttribute(XmlUtil.FILE_ID, id)
       w.writeAttribute(XmlUtil.ABS_PATH, file.getCanonicalPath)
       w.writeAttribute(RUN_START_TIME, ResultMzMLFile.dateFormat.format(runStartTime))
       w.endElement
    }
    
    def writeMzq(i: Int, cv: Cv, rawFilesGroupList: java.util.List[RawFilesGroup], assayList: java.util.List[Assay]) {
    	var rawFilesGroup = new RawFilesGroup()
    	var rawFiles = rawFilesGroup.getRawFile();
    	var rawFile = new RawFile()
    	var assay = new Assay()
    	rawFile.setId("rf_" + i)
    	rawFile.setName(file.getName())
    	rawFile.setLocation(file.getAbsolutePath())
    	rawFiles.add(rawFile);
    	rawFilesGroup.setId("rg_" + i)
    	assay.setId("a_" + i)
    	assay.setName(file.getName)
    	var label = new Label()
    	var cvParam = new CvParam();
    	cvParam.setAccession("MS:1002038")
    	cvParam.setName("unlabeled sample")
    	cvParam.setCv(cv)
    	var modParam = new ModParam()
    	modParam.setCvParam(cvParam)
    	label.getModification().add(modParam)
    	assay.setLabel(label)
    	assay.setRawFilesGroup(rawFilesGroup)
    	assayList.add(assay)
    	rawFilesGroupList.add(rawFilesGroup)
  }
}



class ResultParameter(
		var name:String = "", 
		var value:String = "", 
		r:XmlReader = null
) {
	def this(r:XmlReader) = this("", "", r)
    
    if (r != null) {
        name = r.readAttributeString(XmlUtil.NAME)
        value = r.readAttributeString(XmlUtil.VALUE)
    }

    def write(w:XmlWriter) {
        w.startElement(XmlUtil.PARAM)
        w.writeAttribute(XmlUtil.NAME, name)
        w.writeAttribute(XmlUtil.VALUE, value)
        w.endElement
    }
}



class ResultPrecursor(
		var mz:Double = 0,
	    var proteinName:String = "",
	    var peptideSequence:String = "",
	    var replicates:Array[ResultReplicate] = Array(),
	    r:XmlReader = null
) {
	import ResultFile._
	
	def this(r:XmlReader) = this(0, "", "", Array(), r)

    if (r != null) {
    	mz = r.readAttributeDouble(MZ)
        proteinName = r.readAttributeString(PROTEIN)
        peptideSequence = r.readAttributeString(PEPTIDE_SEQ)
        r.ensure(REPLICATES)
        replicates = new Array(r.readAttributeInt(XmlUtil.COUNT))
        for (i <- 0 until replicates.length) {
            r.ensure(REPLICATE)
            replicates(i) = new ResultReplicate(r)
        }
    }

    def write(w:XmlWriter) {
        w.startElement(PRECURSOR)
        w.writeAttribute(MZ, mz)
        w.writeAttribute(PROTEIN, proteinName)
        w.writeAttribute(PEPTIDE_SEQ, peptideSequence)

        w.startListElement(REPLICATES, replicates)
        for (r <- replicates)
            r.write(w)
        w.endElement
        
        w.endElement
    }
    
    def writeMzq(i: Int, cv: Cv, cv_uo:Cv, assayList: java.util.List[Assay], featureList: Array[java.util.List[Feature]], pepDataMatrix: DataMatrix, qualDataMatrix: DataMatrix, featureDataMatrix: Array[DataMatrix]): PeptideConsensus = {
    var peptideConsensus = new PeptideConsensus()
    peptideConsensus.setId("pep_" + i)
    var charge:Long = 2
    // TODO: Also write out modifications according to mzq standard
    peptideSequence = peptideSequence.replaceAllLiterally("C[+57.0]", "C(UniMod:4)") 
    val pep = UniMod.parseUniModSequence(peptideSequence)
    val pepMass = pep.monoisotopicMass
    if (pepMass>0)
    {
      charge = math.round (pepMass/mz)
    }
    peptideConsensus.getCharge().add(charge.toString());
    peptideConsensus.setPeptideSequence(peptideSequence)
    var evidenceList = peptideConsensus.getEvidenceRef()
    if (replicates.length > 0) {
      var transitions = replicates(0).transitions.length
      var peptideRow = new Row()
      var pValueRow = new Row()
      peptideRow.setObject(peptideConsensus)
      pValueRow.setObject(peptideConsensus)
      for (rep <- 0 until replicates.length) {
        if (replicates(rep).quality.fdr equals Double.NaN) {
          peptideRow.getValue().add("null")
          pValueRow.getValue().add("null")
        } else {
          peptideRow.getValue().add(replicates(rep).totalArea.toString())
          pValueRow.getValue().add(replicates(rep).quality.fdr.toString())
        }
        for (transition <- 0 until replicates(rep).transitions.length) {
          var evidenceRef = new EvidenceRef()
          var feature = new Feature()
          feature.setMz(mz)
          feature.setCharge(charge.toString());
          if (replicates(rep).retentionTime.peak equals Double.NaN)
          {
            feature.setRt("null")
          } else {
        	feature.setRt("" + replicates(rep).retentionTime.peak)
          }
          feature.setId("ft_" + rep + "_" + mz + "_" + replicates(0).transitions(transition).mz)
          var cp = new CvParam()
          //cp.setName("Q3 mz")
          cp.setName("isolation window target m/z")
          cp.setCv(cv)
          //cp.setAccession("MS:100XXXX")
          cp.setAccession("MS:1000827")
          cp.setValue("" + replicates(0).transitions(transition).mz);
          feature.getCvParam().add(cp)
          cp = new CvParam()
          cp.setName("local retention time")
          cp.setCv(cv)
          if (replicates(rep).retentionTime.peak equals Double.NaN)
          {
            cp.setValue("null")
          } else {
        	cp.setValue("" + replicates(rep).retentionTime.peak)
          }
          cp.setAccession("MS:1000895")
          cp.setUnitAccession("UO:0000031")
          cp.setUnitCv(cv_uo)
          cp.setUnitName("minute")
          feature.getCvParam().add(cp)
          featureList(rep).add(feature)
          evidenceRef.setFeature(feature)
          evidenceRef.getAssayRefs().add(assayList.get(rep).getId())
          peptideConsensus.getEvidenceRef().add(evidenceRef)
          var featureRow = new Row()
          featureRow.setObject(feature)
          if (replicates(rep).quality.fdr equals Double.NaN) {
            featureRow.getValue().add("null")           
          } else {
            featureRow.getValue().add(replicates(rep).transitions(transition).area.toString())
          }
          featureDataMatrix(rep).getRow().add(featureRow)
        }
      }
      pepDataMatrix.getRow().add(peptideRow);
      qualDataMatrix.getRow().add(pValueRow);
    }
    return peptideConsensus
  }

  def writeMzqRows(i: Int, dataMatrix: DataMatrix, featureList: java.util.List[Feature]) = {
    var row = new Row()
    row.setObject(featureList.get(0));
    for (replicate <- replicates) {
      row.getValue().add(replicate.totalArea.toString())
    }
    dataMatrix.getRow().add(row);
  }
}



class ResultReplicate(
		var totalArea:Double = -1,
		var fileId:Int = -1,
		var transitions:Array[ResultTransition] = Array(),
	    var retentionTime:ResultRetentionTime = new ResultRetentionTime,
	    var quality:ResultQuality = new ResultQuality,
	    var peakBeforeWindowFlag:Boolean = false,
	    var peakAfterWindowFlag:Boolean = false,
	    r:XmlReader = null
) {
	def this(r:XmlReader) = this(-1, -1, null, null, null, false, false, r)
    import ResultFile._

    if (r != null) {
    	
    	totalArea = r.readAttributeDouble(AREA)
        fileId = r.readAttributeInt(XmlUtil.FILE_ID)
        if (r.hasAttribute(PEAK_BEFORE_WINDOW_FLAG))
        	peakBeforeWindowFlag = true
        if (r.hasAttribute(PEAK_AFTER_WINDOW_FLAG))
        	peakAfterWindowFlag = true
        r.ensure(AREAS)
        transitions = new Array(r.readAttributeInt(XmlUtil.COUNT))
        for (i <- 0 until transitions.length) {
            r.ensure(TRANSITION)
            transitions(i) = new ResultTransition(r)
        }

        r.ensure(RT)
        retentionTime = new ResultRetentionTime(r)

        r.ensure(QUALITY)
        quality = new ResultQuality(r)
    }

    def write(w:XmlWriter) {
        w.startElement(REPLICATE)
        w.writeAttribute(AREA, totalArea)
        w.writeAttribute(XmlUtil.FILE_ID, fileId)
        if (peakBeforeWindowFlag)
        	w.writeAttribute(PEAK_BEFORE_WINDOW_FLAG, "")
        if (peakAfterWindowFlag)
        	w.writeAttribute(PEAK_AFTER_WINDOW_FLAG, "")

        w.startListElement(AREAS, transitions)
        for (t <- transitions)
            t.write(w)
        w.endElement

        retentionTime.write(w)
        quality.write(w)

        w.endElement
    }
}



class ResultTransition(
		var mz:Double = 0, 
		var ion:String = "", 
		var area:Double = -1, 
		r:XmlReader = null
) {
	import ResultFile._
	def this(r:XmlReader) = this(-1, null, -1, r)
    
    if (r != null) {
    	mz = r.readAttributeDouble(MZ)
        ion = r.readAttributeString(ION)
        area = r.readAttributeDouble(AREA)
    }

    def write(w:XmlWriter) {
        w.startElement(TRANSITION)
        w.writeAttribute(MZ, mz)
        w.writeAttribute(ION, ion)
        w.writeAttribute(AREA, area)
        w.endElement
    }
}




class ResultQuality(
		var fdr:Double = -1, 
		var nOConds:Int = 0,
		r:XmlReader = null
) {
	def this(r:XmlReader) = this(-1, -1, r)
	import ResultFile._
	
    if (r != null) {
    	if (r.hasAttribute(FDR)) fdr = r.readAttributeDouble(FDR)
    	else fdr = r.readAttributeDouble(P_VALUE)
        nOConds = r.readAttributeInt(NBR_CONDITIONS)
    }

    def write(xmlWriter:XmlWriter) {
        xmlWriter.startElement(QUALITY)
        xmlWriter.writeAttribute(P_VALUE, fdr)
        xmlWriter.writeAttribute(NBR_CONDITIONS, nOConds)
        xmlWriter.endElement
    }
}




class ResultRetentionTime(
		var start:Double = 0, 
		var peak:Double = 0, 
		var end:Double = 0, 
		r:XmlReader = null
) {
	import ResultFile._
	def this(r:XmlReader) = this(-1, -1, -1, r)
	def width = end - start
	
	if (r != null) {
		start = r.readAttributeDouble(START)
        peak = r.readAttributeDouble(PEAK)
        end = r.readAttributeDouble(END)
    }

    def write(w:XmlWriter) {
        w.startElement(RT)
        w.writeAttribute(START, start)
        w.writeAttribute(PEAK, peak)
        w.writeAttribute(END, end)
        w.endElement()
    }
}