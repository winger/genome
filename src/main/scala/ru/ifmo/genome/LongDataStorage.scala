package ru.ifmo.genome

import java.nio.ByteBuffer
import collection.mutable.ArrayBuffer


/**
 * Author: Vladislav Isenbaev (isenbaev@gmail.com)
 */

class LongDataStorage(var size: Long) {
  import LongDataStorage._

  val segments = ArrayBuffer[ByteBuffer]()

  def ensureCapacity(newSize: Long) {
    size = size max newSize
    while (segments.length * segmentLength < size) {
      segments += ByteBuffer.allocateDirect(segmentLength)
    }
  }
  
  def apply(i: Long): Byte = {
    assert(i < size)
    segments(i / segmentLength intValue).get(i % segmentLength intValue)
  }

  def update(i: Long, v: Byte) {
    segments(i / segmentLength intValue).put(i % segmentLength intValue, v)
  }
}

object LongDataStorage {
  val segmentLength = 16 * 1024 * 1024
}