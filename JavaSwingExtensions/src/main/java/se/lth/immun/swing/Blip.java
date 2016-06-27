/*
 * Blip.java
 * 
 * Written by Joseph Bowbeer and released to the public domain,
 * as explained at http://creativecommons.org/licenses/publicdomain
 */

package se.lth.immun.swing;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Insets;
import static java.awt.RenderingHints.KEY_ANTIALIASING;
import static java.awt.RenderingHints.VALUE_ANTIALIAS_ON;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComponent;
import javax.swing.Timer;

/**
 * Simple animation in the style of a radar screen.
 * Used to indicate that background tasks are running. 
 *
 * @author  Joseph Bowbeer
 * @version 1.0
 */
public class Blip extends JComponent implements ActionListener {

    /**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	/** Delay per frame, in milliseconds. */
    public static final int DELAY = 50;

    /** Sweep increment per frame, in degrees. */
    public static final int ANGLE = 36;

    private final Timer timer = new Timer(DELAY, this);
    private int startAngle;
    private int endAngle;

    /** Creates a blip. */
    public Blip() {
    }

    /** Starts the animation timer. */
    public void start() {
        timer.start();
    }

    /** Stops the animation timer. */
    public void stop() {
        timer.stop();
        startAngle = endAngle = 0;
        repaint();
    }

    /** Advances animation to next frame. Called by timer. */
    public void actionPerformed(ActionEvent e) {
        endAngle += ANGLE;
        if (endAngle > 360) {
            endAngle = 360;
            startAngle += ANGLE;
            if (startAngle > 360) {
                startAngle = endAngle = 0;
            }
        }
        repaint();
    }

    /** Paints the current frame. */
    public void paintComponent(Graphics g) {
        int w = getWidth();
        int h = getHeight();
        if (isOpaque()) {
            g.setColor(getBackground());
            g.fillRect(0, 0, w, h);
            g.setColor(getForeground());
        }
        Insets ins = getInsets();
        w -= (ins.left + ins.right);
        h -= (ins.top + ins.bottom);
        // Antialiasing improves appearance
        ((Graphics2D) g).setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON);
        g.fillArc(ins.left, ins.top, w, h,
            90 - startAngle, startAngle - endAngle);
    }
}
