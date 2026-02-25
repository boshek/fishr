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

