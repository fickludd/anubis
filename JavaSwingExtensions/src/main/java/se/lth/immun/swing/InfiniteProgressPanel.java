package se.lth.immun.swing;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.font.FontRenderContext;
import java.awt.font.TextLayout;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JComponent;
import javax.swing.Timer;

/**
 * Written by Romain Guy as a part of his blog entry "Wait with style in Swing",
 * modified by Ufuk Kirik (minimally) to fit the FEvER project
 * 
 * Big thanks to Romain for sharing this lovely piece of code with the Java community.
 * 
 * @author Romain Guy
 * @author Ufuk Kirik
 */

public class InfiniteProgressPanel extends JComponent implements MouseListener
{
    /**
	 * 
	 */
	private static final long serialVersionUID = 3525721325198156354L;
	protected Area[]  ticker		= null;
    //protected Thread  animation	= null;
    protected Timer	timer		= null;
    protected ActionListener onTick = null;
	protected boolean started		= false;
    protected int     alphaLevel = 0;
    protected int     rampDelay  = 300;
    protected float   shield     = 0.70f;
    protected String  text       = "";
    protected int     barsCount  = 14;
    protected float   fps        = 15.0f;
    protected Integer wid, hei;
    private boolean rampUp = true;
    
    protected RenderingHints hints = null;

    
    
    public InfiniteProgressPanel()
    {
        this("");
    }

    
    
    public InfiniteProgressPanel(String text)
    {
        this(text, 14);
    }
    
    

    public InfiniteProgressPanel(String text, int barsCount)
    {
        this(text, barsCount, 0.70f);
    }

    
    
    public InfiniteProgressPanel(String text, int barsCount, float shield)
    {
        this(text, barsCount, shield, 15.0f);
    }

    
    
    public InfiniteProgressPanel(String text, int barsCount, float shield, float fps)
    {
        this(text, barsCount, shield, fps, 300);
    }

    
    
    public InfiniteProgressPanel(String text, int barsCount, float shield, float fps, int rampDelay)
    {
        this.text 	   = text;
        this.rampDelay = rampDelay >= 0 ? rampDelay : 0;
        this.shield    = shield >= 0.0f ? shield : 0.0f;
        this.fps       = fps > 0.0f ? fps : 15.0f;
        this.barsCount = barsCount > 0 ? barsCount : 14;
        this.onTick = new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                tick();
            }
        };
        this.timer = new Timer(rampDelay, onTick);
        
        this.hints = new RenderingHints(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        this.hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        this.hints.put(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
    }

    
    
    public void setText(String text)
    {
        repaint();
        this.text = text;
    }

    
    
    public String getText()
    {
        return text;
    }

    
    
    public void start()
    {
        addMouseListener(this);
        setVisible(true);
        ticker = buildTicker();
        timer.start();
        //animation = new Thread(new Animator(true), "progress-animator");
        //animation.start();
    }

    
    
    public void stop()
    {
    	timer.stop();
        /*
        if (animation != null) {
	        animation.interrupt();
	        animation = null;
	        animation = new Thread(new Animator(false), "progress-animator");
	        animation.start();
        }
        */
    }
    
    
    
    public void interrupt()
    {
    	timer.stop();
    	removeMouseListener(this);
        setVisible(false);
    	/*
        if (animation != null) {
            animation.interrupt();
            animation = null;

            removeMouseListener(this);
            setVisible(false);
        }
        */
    }

    
    
    public void paintComponent(Graphics g)
    {
        if (started)
        {
            int width  = getWidth();
//            int height = getHeight();
            double maxY = 0.0; 

            Graphics2D g2 = (Graphics2D) g;
            g2.setRenderingHints(hints);
            
            g2.setColor(new Color(255, 255, 255, (int) (alphaLevel * shield)));
            g2.fillRect(0, 0, getWidth(), getHeight());

            for (int i = 0; i < ticker.length; i++)
            {
                int channel = 224 - 128 / (i + 1);
                g2.setColor(new Color(channel, channel, channel, alphaLevel));
                g2.fill(ticker[i]);

                Rectangle2D bounds = ticker[i].getBounds2D();
                if (bounds.getMaxY() > maxY)
                    maxY = bounds.getMaxY();
            }

            if (text != null && text.length() > 0)
            {
	            FontRenderContext context = g2.getFontRenderContext();
	            TextLayout layout = new TextLayout(text, getFont(), context);
	            Rectangle2D bounds = layout.getBounds();
	            g2.setColor(getForeground());
	            layout.draw(g2, (float) (width - bounds.getWidth()) / 2,
	                    		(float) (maxY + layout.getLeading() + 2 * layout.getAscent()));
            }
        }
    }
    
    

    private Area[] buildTicker()
    {
    	if(wid == null || hei == null){
    		wid = getParent().getWidth();
    		hei = getParent().getHeight();
    	}
    	
        Area[] ticker = new Area[barsCount];
        Point2D.Double center = new Point2D.Double((double) wid / 2, (double) hei / 2);
        double fixedAngle = 2.0 * Math.PI / ((double) barsCount);
        
        //System.out.println("## DEBUG (ticker) center location: " + center.toString());

        for (double i = 0.0; i < (double) barsCount; i++)
        {
            Area primitive = buildPrimitive();

            AffineTransform toCenter = AffineTransform.getTranslateInstance(center.getX(), center.getY());
            AffineTransform toBorder = AffineTransform.getTranslateInstance(45.0, -6.0);
            AffineTransform toCircle = AffineTransform.getRotateInstance(-i * fixedAngle, center.getX(), center.getY());

            AffineTransform toWheel = new AffineTransform();
            toWheel.concatenate(toCenter);
            toWheel.concatenate(toBorder);

            primitive.transform(toWheel);
            primitive.transform(toCircle);
            
            ticker[(int) i] = primitive;
        }

        return ticker;
    }
    
    

    private Area buildPrimitive()
    {
        Rectangle2D.Double body = new Rectangle2D.Double(6, 0, 30, 12);
        Ellipse2D.Double   head = new Ellipse2D.Double(0, 0, 12, 12);
        Ellipse2D.Double   tail = new Ellipse2D.Double(30, 0, 12, 12);

        Area tick = new Area(body);
        tick.add(new Area(head));
        tick.add(new Area(tail));

        return tick;
    }
    
    
    
    private void tick()
    {
    	wid = getWidth();
    	hei = getHeight();
        Point2D.Double center = new Point2D.Double((double) wid / 2, (double) hei / 2);
        double fixedIncrement = 2.0 * Math.PI / ((double) barsCount);
        AffineTransform toCircle = AffineTransform.getRotateInstance(fixedIncrement, center.getX(), center.getY());
        
        //System.out.println("## DEBUG (animator) center location: " + center.toString());


        long start = System.currentTimeMillis();
        if (rampDelay == 0)
            alphaLevel = rampUp ? 255 : 0;

        started = true;
        boolean inRamp = rampUp;

        while (!Thread.interrupted())
        {
            if (!inRamp)
            {
                for (int i = 0; i < ticker.length; i++)
                    ticker[i].transform(toCircle);
            }

            repaint();

            if (rampUp)
            {
                if (alphaLevel < 255)
                {
                    alphaLevel = (int) (255 * (System.currentTimeMillis() - start) / rampDelay);
                    if (alphaLevel >= 255)
                    {
                        alphaLevel = 255;
                        inRamp = false;
                    }
                }
            } else if (alphaLevel > 0) {
                alphaLevel = (int) (255 - (255 * (System.currentTimeMillis() - start) / rampDelay));
                if (alphaLevel <= 0)
                {
                    alphaLevel = 0;
                    break;
                }
            }

            try
            {
                Thread.sleep(inRamp ? 10 : (int) (1000 / fps));
            } catch (InterruptedException ie) {
                break;
            }
            Thread.yield();
        }

        if (!rampUp)
        {
            started = false;
            repaint();

            setVisible(false);
            removeMouseListener(InfiniteProgressPanel.this);
        }
    }

    /*
    protected class Animator implements Runnable
    {
        private boolean rampUp = true;

        protected Animator(boolean rampUp)
        {
            this.rampUp = rampUp;
        }

        public void run()
        {
        	wid = getWidth();
        	hei = getHeight();
            Point2D.Double center = new Point2D.Double((double) wid / 2, (double) hei / 2);
            double fixedIncrement = 2.0 * Math.PI / ((double) barsCount);
            AffineTransform toCircle = AffineTransform.getRotateInstance(fixedIncrement, center.getX(), center.getY());
            
            //System.out.println("## DEBUG (animator) center location: " + center.toString());

    
            long start = System.currentTimeMillis();
            if (rampDelay == 0)
                alphaLevel = rampUp ? 255 : 0;

            started = true;
            boolean inRamp = rampUp;

            while (!Thread.interrupted())
            {
                if (!inRamp)
                {
                    for (int i = 0; i < ticker.length; i++)
                        ticker[i].transform(toCircle);
                }

                repaint();

                if (rampUp)
                {
                    if (alphaLevel < 255)
                    {
                        alphaLevel = (int) (255 * (System.currentTimeMillis() - start) / rampDelay);
                        if (alphaLevel >= 255)
                        {
                            alphaLevel = 255;
                            inRamp = false;
                        }
                    }
                } else if (alphaLevel > 0) {
                    alphaLevel = (int) (255 - (255 * (System.currentTimeMillis() - start) / rampDelay));
                    if (alphaLevel <= 0)
                    {
                        alphaLevel = 0;
                        break;
                    }
                }

                try
                {
                    Thread.sleep(inRamp ? 10 : (int) (1000 / fps));
                } catch (InterruptedException ie) {
                    break;
                }
                Thread.yield();
            }

            if (!rampUp)
            {
                started = false;
                repaint();

                setVisible(false);
                removeMouseListener(InfiniteProgressPanel.this);
            }
        }
    }
    */

    public void mouseClicked(MouseEvent e) {
   	}

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }
}
