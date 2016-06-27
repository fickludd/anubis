/*
 * File: SwingWorker.java
 *
 * Written by Joseph Bowbeer and released to the public domain,
 * as explained at http://creativecommons.org/licenses/publicdomain
 */

package se.lth.immun.swing;

import java.awt.EventQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * An abstract class that you subclass to perform GUI-related work
 * in a dedicated thread.
 * <p>
 * This class was adapted from the SwingWorker written by Hans Muller
 * and presented in "Using a Swing Worker Thread" in the Swing
 * Connection. A closely related version of this class is described
 * in "The Last Word in Swing Threads" in the Swing Connection.
 * <p>
 * This SwingWorker implements Future and Runnable. The default executor
 * creates a new thread per task, but this choice can be overridden.
 * <p>
 * <b>Sample Usage</b>
 * <p>
 * <pre>
 * import java.util.concurrent.CancellationException;
 * import java.util.concurrent.ExecutionException;
 * 
 * public class SwingWorkerDemo extends javax.swing.JApplet {
 * 
 *   private static final int TIMEOUT = 5000; // 5 seconds
 *   private javax.swing.JLabel status;
 *   private javax.swing.JButton start;
 *   private javax.swing.Timer timer;
 *   private SwingWorker worker;
 * 
 *   public SwingWorkerDemo() {
 *     status = new javax.swing.JLabel("Ready");
 *     status.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
 *     getContentPane().add(status, java.awt.BorderLayout.CENTER);
 *     start = new javax.swing.JButton("Start");
 *     getContentPane().add(start, java.awt.BorderLayout.SOUTH);
 * 
 *     start.addActionListener(new java.awt.event.ActionListener() {
 *       public void actionPerformed(java.awt.event.ActionEvent evt) {
 *         if (start.getText().equals("Start")) {
 *           start.setText("Stop");
 *           status.setText("Working...");
 *           worker = new DemoSwingWorker();
 *           worker.start();
 *           timer.start();
 *         } else if (worker.cancel(true)) {
 *           status.setText("Cancelled");
 *         }
 *       }
 *     });
 * 
 *     timer = new javax.swing.Timer(TIMEOUT, null);
 *     timer.addActionListener(new java.awt.event.ActionListener() {
 *       public void actionPerformed(java.awt.event.ActionEvent evt) {
 *         if (worker.cancel(true)) {
 *           status.setText("Timed out");
 *         }
 *       }
 *     });
 *     timer.setRepeats(false);
 *   }
 * 
 *   private class DemoSwingWorker extends SwingWorker&lt;String&gt; {
 *     protected String construct() throws InterruptedException {
 *       // Take a random nap. If we oversleep, timer cancels us.
 *       Thread.sleep(new java.util.Random().nextInt(2 * TIMEOUT));
 *       return "Success";
 *     }
 *     protected void finished() {
 *       timer.stop();
 *       start.setText("Start");
 *       try {
 *         status.setText(get());
 *       } catch (CancellationException ex) {
 *         // status was assigned when cancelled 
 *       } catch (ExecutionException ex) {
 *         status.setText("Exception: " + ex.getCause());
 *       } catch (InterruptedException ex) {
 *         // event-dispatch thread won't be interrupted 
 *         throw new IllegalStateException(ex + "");
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * @author  Hans Muller
 * @author  Joseph Bowbeer
 * @version 4.0
 * @see <a href="http://java.sun.com/products/jfc/tsc/articles/threads/threads2.html">Using a Swing Worker Thread</a>
 * @see <a href="http://java.sun.com/products/jfc/tsc/articles/threads/threads3.html">The Last Word in Swing Threads</a>
 */
public abstract class SwingWorker<V> implements Future<V>, Runnable {

    /**
     * Default executor. Executes each task in a new thread.
     */
    private static final Executor EXECUTOR = new Executor() {
        public void execute(Runnable command) {
            new Thread(command).start();
        }
    };

    /** Executor instance. */
    private Executor executor;

    /** <tt>true</tt> if <tt>start</tt> method was called. */
    private boolean started;

    /** Creates new SwingWorker with default executor. */
    public SwingWorker() {
        this(EXECUTOR);
    }

    /**
     * Creates new SwingWorker with specified executor.
     * @param e executor for this worker
     */
    protected SwingWorker(Executor e) {
        setExecutor(e);
    }

    /**
     * Sets executor to be used when worker is started. 
     * @param e executor for this worker
     */
    public synchronized void setExecutor(Executor e) {
        executor = e;
    }

    /**
     * Returns executor to be used when worker is started. 
     * @return executor
     */
    public synchronized Executor getExecutor() {
        return executor;
    }

    /**
     * Submits this worker to executor for execution.
     * @throws RejectedExecutionException if the executor balks  
     */
    public synchronized void start() {
        if (!started) {
            executor.execute(this);
            started = true;
        }
    }

    /**
     * Calls the <tt>construct</tt> method to compute the
     * result, then invokes the <tt>finished</tt> method on
     * the event dispatch thread.
     */
    private final FutureTask<V> task =
        new FutureTask<V>(new Callable<V>() {
            public V call() throws Exception {
                return construct();
            }
        }) {
            protected void done() {
                EventQueue.invokeLater(new Runnable() {
                    public void run() {
                        finished();
                    }
                });
            }
        };

    /**
     * Computes the value to be returned by the <tt>get</tt> method.
     */
    protected abstract V construct() throws Exception;

    /**
     * Called on the event dispatching thread (not on the worker thread)
     * after the <tt>construct</tt> method has returned.
     */
    protected void finished() { }

    /* Runnable implementation. */

    /** Runs this task. Called by executor. */
    public void run() {
        task.run();
    }

    /* Future implementation. */

    /** Attempts to cancel execution of this task. */
    public boolean cancel(boolean mayInterruptIfRunning) {
        return task.cancel(mayInterruptIfRunning);
    }

    /**
     * Returns true if this task was cancelled before it completed normally.
     */
    public boolean isCancelled() {
        return task.isCancelled();
    }

    /** Returns true if this task completed. */
    public boolean isDone() {
        return task.isDone();
    }

    /**
     * Waits if necessary for this task to complete, and then returns its
     * result.
     */
    public V get() throws InterruptedException, ExecutionException {
        return task.get();
    }

    /**
     * Waits if necessary for at most the given time for this task to complete,
     * and then returns its result, if available.
     */
    public V get(long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException {
        return task.get(timeout, unit);
    }
}
