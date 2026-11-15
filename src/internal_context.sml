structure InternalContext =
struct
  val current = ref "running yack"

  fun set phase = current := phase
  fun get () = !current
end
