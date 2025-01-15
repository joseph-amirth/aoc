import java.io.File

const val DIST = 26501365L

// const val DIST = 500L

enum class Dir(val delRow: Int, val delCol: Int) {
  UP(-1, 0),
  DOWN(1, 0),
  LEFT(0, -1),
  RIGHT(0, 1),
}

fun main() {
  val bufferedReader = File("input.txt").bufferedReader()
  val inputString = bufferedReader.use { it.readText() }.trim()

  val grid = inputString.split('\n').map { it.toCharArray().toTypedArray() }.toTypedArray()

  val m = grid.size
  val n = grid.first().size

  val sRow = grid.indexOfFirst { it.contains('S') }
  val sCol = grid[sRow].indexOf('S')

  grid[sRow][sCol] = '.'

  fun isValid(row: Int, col: Int): Boolean {
    return 0 <= row && row < m && 0 <= col && col < n
  }

  fun abstraction(
      corner: Pair<Int, Int>,
      initCornerDist: Int,
      distribution: (cnt: Long) -> Pair<Long, Long>
  ): Long {
    val (cRow, cCol) = corner

    val distance = Array(m) { Array(n) { Int.MAX_VALUE } }
    val queue = ArrayDeque<Pair<Int, Int>>()

    distance[cRow][cCol] = 0
    queue.add(Pair(cRow, cCol))

    while (!queue.isEmpty()) {
      val (row, col) = queue.removeFirst()

      for (dir in Dir.entries) {
        val newRow = row + dir.delRow
        val newCol = col + dir.delCol
        if (isValid(newRow, newCol) &&
            grid[newRow][newCol] == '.' &&
            distance[newRow][newCol] == Int.MAX_VALUE) {
          distance[newRow][newCol] = 1 + distance[row][col]
          queue.addLast(Pair(newRow, newCol))
        }
      }
    }

    var result = 0L
    for (row in 0..<m) {
      for (col in 0..<n) {
        if (distance[row][col] == Int.MAX_VALUE) continue
        val target = DIST - initCornerDist - distance[row][col]
        val (odd, even) = distribution(target / m + 1)
        if (target % 2L == 0L) {
          result += odd
        } else {
          result += even
        }
      }
    }
    // println("($cRow, $cCol) $initCornerDist: $result")
    return result
  }

  val constDistribution = { cnt: Long -> Pair(1L, 0L) }

  val normalDistribution = { cnt: Long -> Pair((cnt + 1) / 2, cnt / 2) }

  val triangleDistribution = { cnt: Long ->
    val odd = ((cnt + 1) / 2) * ((cnt + 1) / 2)
    val even = 2 * (cnt / 2) * (cnt / 2 + 1) / 2
    Pair(odd, even)
  }

  var answer = 0L
  answer += abstraction(Pair(m / 2, n / 2), 0, constDistribution)
  for (row in listOf(0, m - 1)) {
    for (col in listOf(0, n - 1)) {
      answer += abstraction(Pair(row, col), m / 2 + n / 2 + 2, triangleDistribution)
    }
  }
  for (row in listOf(0, m - 1)) {
    answer += abstraction(Pair(row, n / 2), m / 2 + 1, normalDistribution)
  }
  for (col in listOf(0, n - 1)) {
    answer += abstraction(Pair(m / 2, col), n / 2 + 1, normalDistribution)
  }
  println(answer)
}
