package se.lth.immun.swing;
/**
 * Code inspired from several online resources, modified by Ufuk Kirik
 * @author Ufuk Kirik
 * Copyright 2010, 2010 Ufuk Kirik
 * Dept of Immunotechnology, Lund University SWEDEN
 * 
 */

public final class Stopwatch {

  /**
  * Start the stopwatch.
  *
  * @throws IllegalStateException if the stopwatch is already running.
  */
  public void start(){
    if ( fIsRunning ) {
      throw new IllegalStateException("Must stop before calling start again.");
    }
    //reset both start and stop
    fStop = 0;
    fIsRunning = true;
    fHasBeenUsedOnce = true;
    fStart = System.currentTimeMillis();
  }

  /**
  * Stop the stopwatch.
  *
  * @throws IllegalStateException if the stopwatch is not already running.
  */
  public void stop() {
    if ( !fIsRunning ) {
      throw new IllegalStateException("Cannot stop if not currently running.");
    }
    fStop = System.currentTimeMillis();
    fIsRunning = false;
  }

  /**
  * Express the "reading" on the stopwatch.
  *
  * @throws IllegalStateException if the Stopwatch has never been used,
  * or if the stopwatch is still running.
  */
  public String toString() {
    validateIsReadable();
    StringBuilder result = new StringBuilder();
    long timeInMillis = fStop - fStart;
    
    final int days = (int) ( timeInMillis / ( 24L * 60 * 60 * 1000 ) );
    int remdr = (int) ( timeInMillis % ( 24L * 60 * 60 * 1000 ) );

    final int hours = remdr / ( 60 * 60 * 1000 );
    remdr %= 60 * 60 * 1000;

    final int mins = remdr / ( 60 * 1000 );
    remdr %= 60 * 1000;

    final int secs = remdr / 1000;
    final int ms = remdr % 1000;
    
    result.append(days > 0 ? days+":" : "0:");
    result.append(hours > 0 ? (hours > 10 ? hours+":" : "0"+hours+":") : "00:");
    result.append(mins > 0 ? (mins > 10 ? mins+":" : "0"+mins+":") : "00:");
    result.append(secs > 0 ? (secs > 10 ? secs+":" : "0"+secs+":") : "00:");
    result.append(ms);
    return result.toString();
  }

  /**
  * Express the "reading" on the stopwatch as a numeric type.
  *
  * @throws IllegalStateException if the Stopwatch has never been used,
  * or if the stopwatch is still running.
  */
  public long toValue() {
    validateIsReadable();
    return fStop - fStart;
  }

  // PRIVATE ////
  private long fStart;
  private long fStop;

  private boolean fIsRunning;
  private boolean fHasBeenUsedOnce;

  /**
  * Throws IllegalStateException if the watch has never been started,
  * or if the watch is still running.
  */
  private void validateIsReadable() {
    if ( fIsRunning ) {
      String message = "Cannot read a stopwatch which is still running.";
      throw new IllegalStateException(message);
    }
    if ( !fHasBeenUsedOnce ) {
      String message = "Cannot read a stopwatch which has never been started.";
      throw new IllegalStateException(message);
    }
  }
}
