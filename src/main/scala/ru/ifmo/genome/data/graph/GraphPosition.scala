package ru.ifmo.genome.data.graph

/**
 *
 * @author Vladislav Isenbaev (vladislav.isenbaev@odnoklassniki.ru)
 */

sealed abstract class GraphPosition

case class NodeGraphPosition(nodeId: Long) extends GraphPosition

case class EdgeGraphPosition(edgeId: Long, dist: Int) extends GraphPosition