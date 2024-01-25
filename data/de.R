# Daten laden ------------------------------------------------------------------

schweizer_naehrwertdatenbank <- 
  read_delim("data/Schweizer_Nahrwertdatenbank.csv", delim = ";", 
             escape_double = FALSE, trim_ws = TRUE, skip = 2)

# Spalten auswählen ------------------------------------------------------------

food_01_de <- schweizer_naehrwertdatenbank %>% 
  select(ID, Name, Kategorie, `Energie, Kilojoule (kJ)`, `Energie, Kalorien (kcal)`, 
         `Fett, total (g)`, `Fettsäuren, gesättigt (g)`, `Cholesterin (mg)`, 
         `Kohlenhydrate, verfügbar (g)`, `Zucker (g)`, `Nahrungsfasern (g)`, 
         `Protein (g)`, `Folat (µg)`, `Vitamin C (Ascorbinsäure) (mg)`, 
         `Calcium (Ca) (mg)`, `Magnesium (Mg) (mg)`, `Eisen (Fe) (mg)`)

# Character Werte in Numeric transformieren ------------------------------------

food_01_de <- food_01_de %>% 
  mutate_at(c("Fett, total (g)", "Fettsäuren, gesättigt (g)", "Cholesterin (mg)", 
              "Kohlenhydrate, verfügbar (g)", "Zucker (g)", "Nahrungsfasern (g)", 
              "Protein (g)", "Folat (µg)", "Vitamin C (Ascorbinsäure) (mg)", 
              "Calcium (Ca) (mg)", "Magnesium (Mg) (mg)", "Eisen (Fe) (mg)"), 
            as.numeric)

# Spaltennamen umbenennen ------------------------------------------------------

food_01_de <- food_01_de %>% 
  rename("Energie (kJ)" = `Energie, Kilojoule (kJ)`, "Energie (kcal)" = 
           `Energie, Kalorien (kcal)`, "Fett (g)" = `Fett, total (g)`, 
         "Fettsäuren (g)" = `Fettsäuren, gesättigt (g)`, "Kohlenhydrate (g)" = 
           `Kohlenhydrate, verfügbar (g)`, "Vitamin C (mg)" = 
           `Vitamin C (Ascorbinsäure) (mg)`, "Calcium (mg)" = `Calcium (Ca) (mg)`, 
         "Magnesium (mg)" = `Magnesium (Mg) (mg)`, "Eisen (mg)" = `Eisen (Fe) (mg)`)

# Kategorien vereinheitlichen --------------------------------------------------

food_02_de <- food_01_de %>% 
  separate(col = Kategorie, into = c("Kategorie01", "Kategorie02", "Kategorie03"), 
           sep = "/")

vectorKat03 <- unique(food_02_de$Kategorie03)
vectorKat03 <- vectorKat03[!vectorKat03 %in% c(NA, "Geflügel;Fleisch und Innereien", 
                                               "Rind;Fleisch und Innereien", 
                                               "Schwein;Fleisch und Innereien", 
                                               "Kalb;Fleisch und Innereien")]

food_02_de <- food_02_de %>% 
  mutate(Kategorie02 = ifelse(Kategorie03 %in% vectorKat03, Kategorie03, Kategorie02)) %>% 
  as.data.frame()

vectorKat02 <- unique(food_02_de$Kategorie02)
vectorKat02 <- vectorKat02[!vectorKat02 %in% c(NA, 
                                               "Gemüse frisch;Nüsse, Samen und Ölfrüchte", 
                                               "Kalb;Fleisch und Innereien", 
                                               "Schwein;Fleisch und Innereien", 
                                               "Geflügel;Fleisch und Innereien")]

food_02_de <- food_02_de %>% 
  mutate(Kategorie01 = ifelse(Kategorie02 %in% vectorKat02, Kategorie02, Kategorie01)) %>% 
  as.data.frame()

food_02_de <- food_02_de %>% 
  select(!c(Kategorie02, Kategorie03)) %>% 
  rename(Kategorie = Kategorie01) %>% 
  mutate(Kategorie = as.factor(Kategorie)) %>% 
  filter(!Kategorie %in% c("Backzutaten", "Blätterteiggebäcke", "Hefe", 
                           "Sprossen und Keimlinge", "Supplemente", 
                           "Gelier- und Bindemittel", "Zucker und Süssstoffe", 
                           "Fette", "Flocken, Kleie und Keime", "Kräuter", 
                           "Salatsaucen", "Salz, Gewürze und Aromen", 
                           "Öle", "Saucen", "Mehle und Stärke", "Teige", 
                           "Aufstriche", "Fisch", "Gemüse", 
                           "Malzextrakthaltige Getränke", "Orientalische Gerichte", 
                           "Sirup", "Süssgetränke energievermindert", 
                           "Trinkwasser", "Fischerzeugnisse", 
                           "Glacen auf Wasserbasis", "Salzsticks und Bretzel", 
                           "Sonstige alkoholische Getränke", "Tee", "Mayonnaisen")) %>% 
  droplevels() %>% 
  mutate(Kategorie = 
           fct_collapse(Kategorie, 
                        "Sonstige süsse Gerichte" = "Glacen auf Milchbasis", 
                        "Frucht- und Gemüsesäfte" = c("Fruchtsäfte", "Gemüsesäfte"), 
                        "Schokolade und Kakaoerzeugnisse" = "Kakaogetränke", 
                        "Kochwurstware" = "Sonstige Fleisch- und Wurstwaren", 
                        "Sonstige Getreideprodukte" = "Riegel", 
                        "Früchte und Gemüse getrocknet" = c("Gemüse getrocknet", 
                                                            "Früchte getrocknet"), 
                        "Sonstiges Fleisch" = "Fleisch und Innereien", 
                        "Hülsenfrüchte" = "Mais", 
                        "Milch" = "Milch- und Joghurtgetränke", 
                        "Nüsse, Samen und Ölfrüchte" = "Gesalzene Nüsse, Samen, Kerne", 
                        "rezente Gerichte" = "Sonstige salzige")) %>% 
  mutate(Hauptkategorie = 
           fct_collapse(Kategorie,
                        "Fleisch, Fisch und Eier" = 
                          c("Eier", "Süsswasserfische", 
                            "Meeresfrüchte, Krusten- und Schalentiere", 
                            "Sonstige Tierarten", "Fleischersatzprodukte", "Lamm, Schaf", 
                            "Wild", "Kochwurstware", "Sonstiges Fleisch", "Geflügel", 
                            "Brühwurstware", "Meeresfische", "Schwein", "Rind", 
                            "Kalb", "Rohwurstware"),
                        "Gerichte und Fast Food" = 
                          c("Fast Food", "Asiatische Gerichte", "Sonstige süsse Gerichte", 
                            "Eintöpfe und Suppen", "Italienische Gerichte", "Sandwiches", 
                            "rezente Gerichte"),
                        "Getränke" = c("Bier", "Süssgetränke", "Spirituosen", 
                                       "Frucht- und Gemüsesäfte", "Kaffee", "Wein"),
                        "Getreideprodukte und Kartoffeln" = 
                          c("Müesli und Brei", "Reis", 
                            "Müeslimischungen und Frühstückscerealien", "Teigwaren", 
                            "Sonstige Getreideprodukte", "Kartoffeln", "Brote und Brotwaren"), 
                        "Kuchen, Süssigkeiten und Snacks" = 
                          c("Bonbons, Frucht- und Kaugummi", "Sonstige Süssigkeiten", 
                            "Cremen und Pudding", 
                            "Knäckebrote, Zwieback, Crackers und Waffeln", 
                            "Konfitüren und süsse Brotaufstriche", 
                            "Schokolade und Kakaoerzeugnisse", "Kuchen, Torten und Cake", 
                            "Sonstige süsse Backwaren", "Guetzli", "Kuchen und Gratins", 
                            "Sonstige salzige Snacks", "Chips"), 
                        "Milch und Milchprodukte" = 
                          c("Käseerzeugnisse", "Frischkäse und Quark", 
                            "Joghurt und Sauermilch", "Milch", "Milchersatzprodukte", 
                            "Rahm und Butter", "Weichkäse", "Hartkäse"), 
                        "Obst und Gemüse" = 
                          c("Pilze", "Früchte und Gemüse getrocknet", "Salate", 
                            "Hülsenfrüchte", "Nüsse, Samen und Ölfrüchte", 
                            "Früchte gekocht (inkl. Konserven)", "Früchte frisch", 
                            "Gemüse frisch", "Gemüse gekocht (inkl. Konserven)")))

# Neu anordnen der Factor Levels für Variable "Hauptkategorie" -----------------

food_02_de$Hauptkategorie <- 
  factor(x = food_02_de$Hauptkategorie,
         levels = c("Fleisch, Fisch und Eier", "Gerichte und Fast Food", 
                    "Getränke", "Getreideprodukte und Kartoffeln", 
                    "Kuchen, Süssigkeiten und Snacks", "Milch und Milchprodukte", 
                    "Obst und Gemüse"))

# "Kategorie" in Variable speichern & Levels neu anordnen ----------------------

varFleisch <- filter(food_02_de, Hauptkategorie == "Fleisch, Fisch und Eier")
varFleisch$Kategorie <- 
  factor(x = varFleisch$Kategorie, 
         levels = c("Brühwurstware", "Eier", "Fleischersatzprodukte", "Geflügel", 
                    "Kalb", "Kochwurstware", "Lamm, Schaf", "Meeresfische", 
                    "Meeresfrüchte, Krusten- und Schalentiere", "Rind", "Rohwurstware", 
                    "Schwein", "Süsswasserfische", "Wild", "Sonstige Tierarten", 
                    "Sonstiges Fleisch"))

varGericht <- filter(food_02_de, Hauptkategorie == "Gerichte und Fast Food")
varGericht$Kategorie <- 
  factor(x = varGericht$Kategorie, 
         levels = c("Asiatische Gerichte", "Eintöpfe und Suppen", "Fast Food", 
                    "Italienische Gerichte", "rezente Gerichte", "Sandwiches", 
                    "Sonstige süsse Gerichte"))

varGetraenk <- filter(food_02_de, Hauptkategorie == "Getränke")
varGetraenk$Kategorie <- 
  factor(x = varGetraenk$Kategorie, 
         levels = c("Bier", "Frucht- und Gemüsesäfte", "Kaffee", "Spirituosen", 
                    "Süssgetränke", "Wein"))

varGetreide <- filter(food_02_de, Hauptkategorie == "Getreideprodukte und Kartoffeln")
varGetreide$Kategorie <- 
  factor(x = varGetreide$Kategorie, 
         levels = c("Brote und Brotwaren", "Kartoffeln", "Müesli und Brei", 
                    "Müeslimischungen und Frühstückscerealien", "Reis", "Teigwaren", 
                    "Sonstige Getreideprodukte"))

varKuchen <- filter(food_02_de, Hauptkategorie == "Kuchen, Süssigkeiten und Snacks")
varKuchen$Kategorie <- 
  factor(x = varKuchen$Kategorie, 
         levels = c("Bonbons", "Chips", "Cremen und Pudding", "Frucht- und Kaugummi", 
                    "Guetzli", "Knäckebrote", "Konfitüren und süsse Brotaufstriche", 
                    "Kuchen, Torten und Cake", "Kuchen und Gratins", 
                    "Schokolade und Kakaoerzeugnisse", "Zwieback, Crackers und Waffeln", 
                    "Sonstige salzige Snacks", "Sonstige süsse Backwaren", 
                    "Sonstige Süssigkeiten"))

varMilch <- filter(food_02_de, Hauptkategorie == "Milch und Milchprodukte")
varMilch$Kategorie <- 
  factor(x = varMilch$Kategorie, 
         levels = c("Frischkäse und Quark", "Hartkäse", "Joghurt und Sauermilch", 
                    "Käseerzeugnisse", "Milch", "Milchersatzprodukte", 
                    "Rahm und Butter", "Weichkäse"))

varOuG <- filter(food_02_de, Hauptkategorie == "Obst und Gemüse")
varOuG$Kategorie <- 
  factor(x = varOuG$Kategorie, 
         levels = c("Früchte frisch", "Früchte gekocht (inkl. Konserven)", 
                    "Früchte und Gemüse getrocknet", "Gemüse frisch", 
                    "Gemüse gekocht (inkl. Konserven)", "Hülsenfrüchte", 
                    "Nüsse, Samen und Ölfrüchte", "Pilze", "Salate"))

# Werte für Variable selX & selY speichern -------------------------------------

varMerkmal <- names(food_02_de)
varMerkmal <- 
  varMerkmal[!varMerkmal %in% c("ID", "Name", "Kategorie", "Folat (µg)", 
                                  "Hauptkategorie")]

# Datensatz für Tabelle erstellen ----------------------------------------------
# nicht mehr notwendig

# food_03_de <- food_02_de %>% 
#   select(!c("ID", "Kategorie", "Fettsäuren (g)", "Cholesterin (mg)", "Nahrungsfasern (g)", 
#             "Folat (µg)", "Vitamin C (mg)", "Calcium (mg)", "Magnesium (mg)", 
#             "Eisen (mg)", "Hauptkategorie"))
