/*
 * InvokeUtils.java
 * 
 * Written by Joseph Bowbeer and released to the public domain,
 * as explained at http://creativecommons.org/licenses/publicdomain
 */

package se.lth.immun.swing;

import java.awt.Component;
import java.awt.EventQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

import javax.swing.Icon;
import javax.swing.JOptionPane;

/**
 * More invoke-and-wait utilities for Swing. Background threads may
 * use these methods to prompt the user for input on the event-dispatch
 * thread, and return the response to the background thread. 
 * <p>
 * <b>Sample Usage</b>
 * <p>
 * <pre>
 * import java.awt.BorderLayout;
 * import java.awt.Dimension;
 * import javax.swing.JApplet;
 * import javax.swing.JLabel;
 * import javax.swing.JOptionPane;
 * 
 * import jozart.swingutils.SwingWorker;
 * 
 * public class InvokeDialogTest extends JApplet {
 * 
 *     private JLabel jLabel;
 * 
 *     private SwingWorker&lt;Object&gt; worker;
 * 
 *     public InvokeDialogTest() {
 *         jLabel = new JLabel();
 *         jLabel.setPreferredSize(new Dimension(200, 100));
 *         jLabel.setHorizontalAlignment(JLabel.CENTER);
 *         getContentPane().add(jLabel, BorderLayout.CENTER);
 *     }
 * 
 *     public synchronized void start() {
 *         jLabel.setText("Running...");
 *         worker = new SwingWorker&lt;Object&gt;() {
 *             // Prompt the user every three seconds
 *             protected Object construct() throws InterruptedException {
 *                 while (true) {
 *                     Thread.sleep(3000);
 *                     int n = InvokeUtils.invokeConfirmDialog(
 *                             jLabel,
 *                             "Operation timed out. Try again?",
 *                             "Timeout",
 *                             JOptionPane.YES_NO_OPTION,
 *                             JOptionPane.WARNING_MESSAGE);
 *                     if (n != JOptionPane.YES_OPTION) {
 *                         break;
 *                     }
 *                 }
 *                 return null;
 *             }
 *             protected void finished() {
 *                 jLabel.setText("Stopped");
 *             }
 *         };
 *         worker.start();
 *     }
 * 
 *     public synchronized void stop() {
 *         if (worker != null) {
 *             worker.cancel(true);
 *             worker = null;
 *         }
 *     }
 * }
 * </pre>
 *  
 * @author  Joseph Bowbeer
 * @version 2.0
 */
public class InvokeUtils {

    /** Tool class. Do not instantiate. */
    private InvokeUtils() { }

    /**
     * Invokes a {@link Callable} function on the AWT event dispatching thread
     * and returns the result to the caller. This call will block until all
     * pending AWT events have been processed, and <tt>function.call()</tt>
     * returns. This method should not be called from the event dispatching
     * thread.
     * <p>
     * Note: If <tt>function.call()</tt> throws an uncaught exception (on the
     * event dispatching thread), it is caught and rethrown as an
     * {@link ExecutionException} on the caller's thread.
     * 
     * @throws InterruptedException
     *             if calling thread is interrupted while waiting for
     *             <tt>function.call()</tt> to return
     * @throws ExecutionException
     *             if <tt>function.call()</tt> throws an exception
     * @throws Error if called from the event dispatching thread             
     */
    public static <V> V invokeAndWait(Callable<V> function)
        throws InterruptedException, ExecutionException {

        if (EventQueue.isDispatchThread()) {
            throw new Error("Cannot call invokeAndWait from the event dispatcher thread");
        }

        FutureTask<V> task = new FutureTask<V>(function);
        EventQueue.invokeLater(task);
        return task.get();
    }

    /**
     * Invokes
     * {@link JOptionPane#showConfirmDialog(Component, Object, String, int, int) JOptionPane.showConfirmDialog}
     * on the AWT event dispatching thread and returns the result to the caller.
     * 
     * @throws InterruptedException
     *             if calling thread is interrupted while waiting for user to confirm
     * @see #invokeAndWait invokeAndWait
     */
    public static int invokeConfirmDialog(Component parentComponent,
            Object message, String title, int optionType, int messageType)
            throws InterruptedException {
        return invokeOptionDialog(parentComponent, message, title, optionType,
                messageType, null, null, null);
    }

    /**
     * Invokes
     * {@link JOptionPane#showOptionDialog(Component, Object, String, int, int, Icon, Object[], Object) JOptionPane.showOptionDialog}
     * on the AWT event dispatching thread and returns the result to the caller.
     * 
     * @throws InterruptedException
     *             if calling thread is interrupted while waiting for user to
     *             select an option
     * @see #invokeAndWait invokeAndWait
     */
    public static int invokeOptionDialog(final Component parentComponent,
            final Object message, final String title, final int optionType,
            final int messageType, final Icon icon, final Object[] options,
            final Object initialValue) throws InterruptedException {

        Callable<Integer> showOptionDialog = new Callable<Integer>() {
            public Integer call() {
                int n = JOptionPane.showOptionDialog(parentComponent, message,
                        title, optionType, messageType, icon, options,
                        initialValue);
                return new Integer(n);
            }
        };

        try {
            return invokeAndWait(showOptionDialog).intValue();
        } catch (ExecutionException e) {
            /*
             * showOptionDialog doesn't throw checked exceptions
             * so ex must be a RuntimeException or an Error.
             */
            Throwable ex = e.getCause();
            if (ex instanceof RuntimeException) {
                throw (RuntimeException) ex;
            } else {
                throw (Error) ex;
            }
        }
    }

    /**
     * Invokes
     * {@link JOptionPane#showInputDialog(Component, Object, String, int) JOptionPane.showInputDialog}
     * on the AWT event dispatching thread and returns the result to the caller.
     * 
     * @throws InterruptedException
     *             if calling thread is interrupted while waiting for user
     *             to input a value
     * @see #invokeAndWait invokeAndWait
     */
    public static Object invokeInputDialog(Component parentComponent,
            Object message, String title, int messageType)
            throws InterruptedException {
        return invokeInputDialog(parentComponent, message, title, messageType,
                null, null, null);
    }

    /**
     * Invokes
     * {@link JOptionPane#showInputDialog(Component, Object, String, int, Icon, Object[], Object) JOptionPane.showInputDialog}
     * on the AWT event dispatching thread and returns the result to the caller.
     * 
     * @throws InterruptedException
     *             if calling thread is interrupted while waiting for user
     *             to input a value
     * @see #invokeAndWait invokeAndWait
     */
    public static Object invokeInputDialog(final Component parentComponent,
            final Object message, final String title, final int messageType,
            final Icon icon, final Object[] selectionValues,
            final Object initialSelectionValue) throws InterruptedException {

        Callable<Object> showInputDialog = new Callable<Object>() {
            public Object call() {
                return JOptionPane.showInputDialog(parentComponent, message,
                        title, messageType, icon, selectionValues,
                        initialSelectionValue);
            }
        };

        try {
            return invokeAndWait(showInputDialog);
        } catch (ExecutionException e) {
            /*
             * showOptionDialog doesn't throw checked exceptions
             * so ex must be a RuntimeException or an Error.
             */
            Throwable ex = e.getCause();
            if (ex instanceof RuntimeException) {
                throw (RuntimeException) ex;
            } else {
                throw (Error) ex;
            }
        }
    }
}
