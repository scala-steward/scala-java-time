package org.threeten.bp

import java.util.NavigableMap

object Platform {
  type NPE = NullPointerException
  type DFE = IndexOutOfBoundsException
  type CCE = ClassCastException

  /**
   * Returns `true` if and only if the code is executing on a JVM. Note: Returns `false` when
   * executing on any JS VM.
   */
  final val executingInJVM = true
}
