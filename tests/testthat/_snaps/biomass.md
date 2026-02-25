# biomass_index throws error on invalid input

    Code
      biomass_index("ten", 5)
    Condition
      Error in `cpue * area_swept`:
      ! non-numeric argument to binary operator

---

    Code
      biomass_index(10, "five")
    Condition
      Error in `cpue * area_swept`:
      ! non-numeric argument to binary operator

