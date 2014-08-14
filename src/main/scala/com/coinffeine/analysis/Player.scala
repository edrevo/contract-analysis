package com.coinffeine.analysis

sealed trait Player {
  def otherPlayer: Player
}

case object Sam extends Player {
  override def otherPlayer = Bob
}

case object Bob extends Player {
  override def otherPlayer = Sam
}
