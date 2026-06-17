setup_effects <- function(effects, all_combos = FALSE) {
  
  if(all_combos) {
    
    no_effect_string <- "no effects"
    has_no_effects <- no_effect_string %in% effects
    
    if (has_no_effects) {
      # --- Handle the "no effects" special case ---
      
      # 1. Start the list with "no effects" as its own element
      no_effect_element <- list(no_effect_string) 
      
      # 2. Get all *other* effects
      other_effects <- effects[effects != no_effect_string]
      
      # 3. Generate combinations *only* if there are other effects
      if (length(other_effects) > 0) {
        combos_list <- lapply(1:length(other_effects), function(m) {
          combn(other_effects, m, simplify = FALSE)
        })
        other_combos <- unlist(combos_list, recursive = FALSE)
      } else {
        # No other effects, so no other combinations
        other_combos <- list() 
      }
      
      # 4. Combine and return
      c(no_effect_element, other_combos) 
      
    } else {
      # --- Original logic: "no effects" is not present ---
      combos_list <- lapply(1:length(effects), function(m) {
        combn(effects, m, simplify = FALSE)
      })
      unlist(combos_list, recursive = FALSE) 
    }
    
  } else {
    # --- Original logic: all_combos is FALSE ---
    list(effects)
  }
  
}