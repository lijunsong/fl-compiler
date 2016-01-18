"/* error: undef field type */ let type a = {x : int, y : nonexists} in 1 end"

  "/* error: undef array of */ let type a = array of nonexists in 1 end"
