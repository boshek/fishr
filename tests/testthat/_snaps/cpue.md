# cpue errors when input is not numeric

    Code
      cpue("five", 10)
    Condition
      Error:
      ! 'catch' must be numeric, got character.

# cpue warns when catch and effort lengths differ

    Code
      cpue(c(100, 200, 300), c(10, 20))
    Condition
      Warning in `catch / effort`:
      longer object length is not a multiple of shorter object length
    Output
      [1] 10 10 30

# cpue uses verbosity when option set to TRUE

    Code
      cpue(100, 10)
    Message
      Processing 1 records
    Output
      [1] 10

