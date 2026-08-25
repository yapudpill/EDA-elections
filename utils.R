# Utility functions

format_names <- function(name) {
  name |>
    str_replace_all("_", " ") |>
    str_to_title()
}

candidates <- function(df, keep = NULL) {
  n <- names(df)
  i2 <- length(n) - if ("geo_shape" %in% n) 2 else 0
  df |>
    select({{keep}}, all_of(16:i2)) |>
    select(where(\(col) !all(col == 0)))
}

top_n <- function(df, n, start = 1) {
  sums <- df |> select(all_of(start:last_col())) |> map_int(sum) |> sort(decreasing = TRUE)
  top <- names(sums[1:n]) |> discard(is.na)
  select(df, seq_len(start - 1), all_of(top))
}

collect_election <- function(type, year, round) {
  elections |>
    filter(type == .env$type, year == .env$year, round == .env$round) |>
    pluck("data") |>
    bind_rows() |>
    filter(!str_ends(id_bvote, "99")) |> # remove the station for prisoners
    mutate(across(where(is.numeric), \(x) replace_na(x, 0)))
}

# Political labels for the candidates of the 2008 municipal elections
dico_2008 <- tibble(
  candidat = c(
    # Gauche
    "dagoma seybah", "wieviorka sylvie", "aidenbaum pierre", "bertinotti dominique",
    "cohen-solal lyne", "bravo jacques", "féraud rémi", "bloche patrick",
    "blumenthal michèle", "coumet jérôme", "castagnou pierre", "hidalgo anne",
    "lepetit annick", "vaillant daniel", "madec roger", "calandra frédérique",
    "lévy romain", "mano jean-yves", "charzat michel",

    # Droite
    "legaret jean-françois", "lekieffre christophe", "roger vincent", "tiberi jean",
    "lecoq jean-pierre", "dati rachida", "lebel françois", "lellouche pierre",
    "burkli delphine", "carrère-gée marie-claire", "goujon philippe", "goasguen claude",
    "de panafieu françoise", "weill-raynal martine", "tissot claude-annick",
    "vasseur véronique", "alphand david", "decorte roxane", "giannesini jean-jacques",

    # Centre
    "girard laurence", "rançon-cavenel heidi", "asmani lynda", "cavada jean-marie",
    "aziere eric", "de sarnez marielle", "delamare raoul",

    # Ecologiste
    "boutault jacques", "dubarry véronique", "dutrey rené", "garel sylvain", "baupin denis"
  ),
  parti = c(
    rep("Gauche", 19),
    rep("Droite", 19),
    rep("Centre", 7),
    rep("Ecologiste", 5)
  )
)

# Political labels for the candidates of the 2020 municipal elections
dico_2020 <- tibble(
  candidat = c(
    # Gauche
    "m. weil ariel", "mme lemardeley marie-christine", "m. ngatcha arnaud", "mme cordebard alexandra",
    "m. vauglin françois", "m. grégoire emmanuel", "m. coumet jérôme", "mme petit carine",
    "mme toranian anouch", "mme taïeb karen", "m. lejoindre eric", "m. dagnaud françois",
    "m. pliez eric", "mme mazetier sandrine", "mme sebbah hanna", "mme marre béatrice",
    "m. elalouf lucas", "m. dhorasoo vikash", "mme legrain sarah", "mme simonnet danielle",

    # Droite
    "m. véron aurélien", "mme berthout florence", "m. lecoq jean-pierre", "mme dati rachida",
    "mme d'hauteserre jeanne", "mme segond sophie", "mme montandon valérie", "mme carrere-gee marie-claire",
    "m. goujon philippe", "mme evren agnès", "m. szpiner francis", "m. boulard geoffroy",
    "m. maurin pierre", "m. fort bertil", "mme garnier nelly", "m. olivier jean-baptiste",
    "m. granier rudolph", "m. didier françois-marie",

    # Centre
    "m. rupin pacôme", "mme bürkli delphine", "mme ibled catherine", "m. bournazel pierre-yves",
    "m. gantzer gaspard", "mme hervieu céline", "m. missoffe alexandre", "m. amellal karim",
    "m. poitoux guillaume", "m. peng chang hua", "m. aziere éric", "mme buzyn agnès",
    "mme toubiana marie", "m. rouxel olivier", "mme calandra frédérique",

    # Ecologiste
    "mme rémy-leleu raphaëlle", "m. raifaud sylvain", "m. belliard david", "mme pierre-marie emmanuelle",
    "mme souyris anne", "mme boux anne-claire", "m. lert dan", "mme guhl antoinette"
  ),
  parti = c(
    rep("Gauche", 20),
    rep("Droite", 18),
    rep("Centre", 15),
    rep("Ecologiste", 8)
  )
)

# Merge the two dictionaries
dico <- bind_rows(dico_2008, dico_2020)
