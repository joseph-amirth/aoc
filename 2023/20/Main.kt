import java.io.File

fun String.toModule(): Pair<String, Char?> {
  if (this.startsWith("&")) {
    return Pair(this.substring(1), '&')
  } else if (this.startsWith("%")) {
    return Pair(this.substring(1), '%')
  }
  return Pair(this, null)
}

fun findSccs(graph: Map<String, List<String>>): List<List<String>> {
  val sccs = mutableListOf<List<String>>()

  val curScc = mutableListOf<String>()

  fun isConnected(u: String, v: String): Boolean {
    val visited = graph.mapValues { false }.toMutableMap()
    fun dfs(x: String) {
      visited[x] = true
      for (neighbor in graph.getOrDefault(x, listOf())) {
        if (!visited.getOrDefault(neighbor, false)) {
          dfs(neighbor)
        }
      }
    }

    dfs(u)
    return visited.getOrDefault(v, false)
  }

  for ((u, _) in graph) {
    if (u in sccs.flatMap { it }) {
      continue
    }
    curScc.add(u)
    for ((v, _) in graph) {
      if (v != u && isConnected(u, v) && isConnected(v, u)) {
        curScc.add(v)
      }
    }
    sccs.add(curScc.map { it })
    curScc.clear()
  }

  return sccs
}

fun main() {
  val bufferedReader = File("input.txt").bufferedReader()
  val inputString = bufferedReader.use { it.readText() }.trim()

  val modules: Map<String, Pair<Char?, List<String>>> =
      inputString
          .split('\n')
          .map { it.split(" -> ") }
          .map { Pair(it[0].toModule(), it[1].split(", ")) }
          .map { Pair(it.first.first, Pair(it.first.second, it.second)) }
          .toMap()

  val graph = modules.mapValues { (key, value) -> value.second }
  val sccs = findSccs(graph)
  println(sccs)

  val inputModules: Map<String, List<String>> =
      modules
          .flatMap { (key, value) -> value.second.map { Pair(it, key) } }
          .groupBy(keySelector = { it.first }, valueTransform = { it.second })

  val states: Map<String, Array<Boolean>> =
      modules.mapValues { (key, value) ->
        when (value.first) {
          '&' -> Array(inputModules[key]!!.size) { false }
          '%' -> Array(1) { false }
          null -> Array(0) { false }
          else -> throw IllegalStateException("WTF")
        }
      }

  val sccStates = sccs.map { mutableMapOf<List<Boolean>, Int>() }
  val sccStatesFound = sccs.map { false }.toMutableList()
  val sccId = sccs.flatMapIndexed { i, it -> it.map { Pair(it, i) } }.toMap()

  generateSequence(1) { it + 1 }
      .take(1 shl 22)
      .forEach { rep ->
        val queue = ArrayDeque<Triple<String, Boolean, String>>()
        for (input in modules["broadcaster"]!!.second) {
          queue.addLast(Triple("broadcaster", false, input))
        }

        while (!queue.isEmpty()) {
          val (src, signal, dest) = queue.removeFirst()
          if (!modules.contains(dest)) {
            continue
          }
          val (type, outputs) = modules[dest]!!
          when (type) {
            '&' -> {
              val index = inputModules[dest]!!.indexOf(src)
              states[dest]!![index] = signal
              val outputSignal = !states[dest]!!.all { it }
              for (output in outputs) {
                queue.addLast(Triple(dest, outputSignal, output))
              }
            }
            '%' -> {
              if (!signal) {
                val state = !states[dest]!![0]
                states[dest]!![0] = state
                for (output in outputs) {
                  queue.addLast(Triple(dest, state, output))
                }
              }
            }
            else -> throw IllegalStateException("WTF")
          }
        }

        for ((sccId, scc) in sccs.withIndex()) {
          val sccState = scc.flatMap { states[it]!!.toList() }
          if (!sccStatesFound[sccId] && sccStates[sccId].contains(sccState)) {
            println("$scc ${sccStates[sccId][sccState]!!} $rep")
            sccStatesFound[sccId] = true
          } else {
            sccStates[sccId][sccState] = rep
          }
        }
      }
}
