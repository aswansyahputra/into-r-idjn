library(tidyverse)

elements_by_episode <-
  read_csv(
    "https://raw.githubusercontent.com/fivethirtyeight/data/master/bob-ross/elements-by-episode.csv"
  )

tidy_elements <-
  elements_by_episode %>%
  janitor::clean_names() %>%
  gather("element", "present", -episode, -title) %>%
  mutate(
    title = str_remove_all(title, '"') %>%
      str_to_title(),
    element = str_replace_all(element, "_", " ") %>%
      str_to_title()
  )

non_bob_ross_painting <-
  tidy_elements %>%
  dplyr::filter(element %in% c("Guest", "Steve Ross"),
                present == 1) %>%
  distinct(episode)

bob_ross_elements <-
  tidy_elements %>%
  anti_join(non_bob_ross_painting) %>%
  group_by(element) %>%
  summarise(n = sum(present),
            pct = 100 * mean(present)) %>%
  arrange(desc(n)) %>%
  dplyr::filter(n > 5)

write_csv(bob_ross_elements, "data/bob_ross_elements.csv")