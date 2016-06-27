#!/usr/bin/python

import os
import sys


cmd = "install"
if len(sys.argv) > 1:
	cmd = sys.argv[1]


if os.system("mvn -version") != 0:
	print "maven not installed!"
	exit(1)

def mvn(x):
	os.chdir(x)
	print "#" * 80
	os.system("mvn %s" % cmd)
	os.chdir("..")
	
if cmd == "install" or cmd == "clean" or cmd == "test":
	mvn("anubis-res-file")
	mvn("anubis-ref-file")
	mvn("anubis-lib")
	mvn("anubis-heat-mapper")
        mvn("App")
        mvn("JavaSwingExtensions")
	mvn("anubis")
	mvn("anubis-ref-creator")
else:
	print "unknown mvn command '%s'" % cmd
	exit(1)
