library(tidyverse)
library(janitor)

elements_by_episode <-
  read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/data/master/bob-ross/elements-by-episode.csv"
  )

tidy_elements <-
  elements_by_episode %>% 
  clean_names() %>% 
  pivot_longer(cols = c(apple_frame:wood_framed), names_to = "element", values_to = "present") %>% 
  mutate(
    title = str_remove_all(title, '"'),
    element = str_replace_all(element, "_", " ")
  ) %>% 
  mutate_if(is.character, str_to_upper) 

non_bob_ross_painting <-
  tidy_elements %>%
  filter(element %in% c("GUEST", "STEVE ROSS"),
                present == 1) %>% 
  distinct(episode)

bob_ross_elements <-
  tidy_elements %>%
  anti_join(non_bob_ross_painting) %>%
  group_by(element) %>%
  summarise(n = sum(present),
            pct = 100 * mean(present)) %>%
  arrange(desc(n)) %>%
  filter(n > 5)

write_csv(bob_ross_elements, "data/bob-ross-elements.csv")