package org.example

import java.io.File

data class GridPosition(val x_pos: Int, val y_pos: Int)

class FrequencyGroup(
        private val antennaPositions: MutableList<GridPosition> = mutableListOf(),
) {
  fun getAntennaPositions(): List<GridPosition> = antennaPositions.toList()

  fun addAntennaPosition(position: GridPosition) {
    antennaPositions.add(position)
  }

  fun getAntinodes(gridWidth: Int, gridHeight: Int): Set<GridPosition> {
    fun isWithinGrid(x: Int, y: Int): Boolean = x in 0 until gridWidth && y in 0 until gridHeight

    fun extendAntinodes(start: GridPosition, end: GridPosition): Set<GridPosition> {
      val dx = end.x_pos - start.x_pos
      val dy = end.y_pos - start.y_pos

      return generateSequence(end) { pos ->
                GridPosition(pos.x_pos + dx, pos.y_pos + dy).takeIf {
                  isWithinGrid(it.x_pos, it.y_pos)
                }
              }
              .toSet()
    }

    return antennaPositions
            .asSequence()
            .flatMapIndexed { i, first ->
              antennaPositions.drop(i + 1).map { second -> first to second }
            }
            .flatMap { (pos1, pos2) ->
              extendAntinodes(pos1, pos2) union extendAntinodes(pos2, pos1)
            }
            .toSet()
  }
}

class FrenquencyMap(
        val frequencyGroups: HashMap<Char, FrequencyGroup> = HashMap(),
) {
  fun getAllAntiNodes(gridWidth: Int, gridHeight: Int): Set<GridPosition> {
    return this.frequencyGroups.values.fold(emptySet()) { acc, group ->
      acc union group.getAntinodes(gridWidth, gridHeight)
    }
  }
}

class RadioGrid(
        val frequencyMap: FrenquencyMap,
        val width: Int,
        val height: Int,
) {
  fun numAntinodes(): Int {
    return this.frequencyMap.getAllAntiNodes(this.width, this.height).size
  }
}

fun initializeRadioGrid(gridData: List<String>): RadioGrid {
  val gridHeight = gridData.size
  val gridWidth = gridData.firstOrNull()?.length ?: 0

  val frequencyMap =
          FrenquencyMap(
                  gridData.joinToString("")
                          .filterNot { it == '.' }
                          .toSet()
                          .associateWith { FrequencyGroup() }
                          .let { HashMap(it) }
          )

  gridData.asSequence()
          .flatMapIndexed { row, line ->
            line.mapIndexedNotNull { col, freq ->
              if (freq != '.') GridPosition(row, col) to freq else null
            }
          }
          .forEach { (pos, freq) -> frequencyMap.frequencyGroups[freq]?.addAntennaPosition(pos) }

  return RadioGrid(frequencyMap, gridWidth, gridHeight)
}

fun main() {
  val gridData: List<String> = File("../inputs.txt").readLines()
  val radioGrid = initializeRadioGrid(gridData)
  println(radioGrid.numAntinodes())
}
