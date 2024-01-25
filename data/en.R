# Daten laden ------------------------------------------------------------------

swiss_food_composition_database <- 
  read_delim("data/Swiss_food_composition_database.csv",
             delim = ";", escape_double = FALSE, trim_ws = TRUE, skip = 2)

# Spalten auswählen ------------------------------------------------------------

food_01 <- swiss_food_composition_database %>% 
  select(ID, Name, Category, `Energy, kilojoules (kJ)`, `Energy, kilocalories (kcal)`, 
         `Fat, total (g)`, `Fatty acids, saturated (g)`, `Cholesterol (mg)`, 
         `Carbohydrates, available (g)`, `Sugars (g)`, `Dietary fibres (g)`, 
         `Protein (g)`, `Folate (µg)`, `Vitamin C (ascorbic acid) (mg)`, 
         `Calcium (Ca) (mg)`, `Magnesium (Mg) (mg)`, `Iron (Fe) (mg)`)

# Character Werte in Numeric transformieren ------------------------------------

food_01 <- food_01 %>% 
  mutate(`Fat, total (g)` = as.numeric(`Fat, total (g)`), `Fatty acids, saturated (g)` = 
           as.numeric(`Fatty acids, saturated (g)`), `Cholesterol (mg)` = 
           as.numeric(`Cholesterol (mg)`), `Carbohydrates, available (g)` = 
           as.numeric(`Carbohydrates, available (g)`), `Sugars (g)` = 
           as.numeric(`Sugars (g)`), `Dietary fibres (g)` = as.numeric(`Dietary fibres (g)`), 
         `Protein (g)` = as.numeric(`Protein (g)`), `Folate (µg)` = as.numeric(`Folate (µg)`), 
         `Vitamin C (ascorbic acid) (mg)` = as.numeric(`Vitamin C (ascorbic acid) (mg)`), 
         `Calcium (Ca) (mg)` = as.numeric(`Calcium (Ca) (mg)`), `Magnesium (Mg) (mg)` = 
           as.numeric(`Magnesium (Mg) (mg)`), `Iron (Fe) (mg)` = as.numeric(`Iron (Fe) (mg)`))

# Spaltennamen umbenennen ------------------------------------------------------

food_01 <- food_01 %>% 
  rename("Energy (kJ)" = `Energy, kilojoules (kJ)`, "Energy (kcal)" = 
           `Energy, kilocalories (kcal)`, "Fat (g)" = `Fat, total (g)`, 
         "Fatty acids (g)" = `Fatty acids, saturated (g)`, "Carbohydrates (g)" = 
           `Carbohydrates, available (g)`, "Vitamin C (mg)" = 
           `Vitamin C (ascorbic acid) (mg)`, "Calcium (mg)" = `Calcium (Ca) (mg)`,
         "Magnesium (mg)" = `Magnesium (Mg) (mg)`, "Iron (mg)" = `Iron (Fe) (mg)`)

# Kategorien vereinheitlichen --------------------------------------------------

food_02 <- food_01 %>% 
  separate(col = Category, into = c("Category01", "Category02", "Category03"), 
           sep = "/")

vectorCat03 <- unique(food_02$Category03)
vectorCat03 <- vectorCat03[!vectorCat03 %in% c(NA, "Veal;Meat and offal", 
                                               "Beef;Meat and offal", 
                                               "Poultry;Meat and offal", 
                                               "Pork;Meat and offal", "Beef")]

food_02 <- food_02 %>% 
  mutate(Category02 = ifelse(Category03 %in% vectorCat03, Category03, Category02)) %>% 
  as.data.frame()

vectorCat02 <- unique(food_02$Category02)
vectorCat02 <- vectorCat02[!vectorCat02 %in% c(NA, 
                                               "Fresh vegetables;Nuts, seeds and oleaginous fruit", 
                                               "Pork;Meat and offal", 
                                               "Poultry;Meat and offal", 
                                               "Veal;Meat and offal")]

food_02 <- food_02 %>% 
  mutate(Category01 = ifelse(Category02 %in% vectorCat02, Category02, Category01)) %>% 
  as.data.frame()

food_02 <- food_02 %>% 
  select(!c(Category02, Category03)) %>% 
  rename(Category = Category01) %>% 
  mutate(Category = as.factor(Category)) %>% 
  filter(!Category %in% c("Baking ingredients", "Puff pastry snacks", "Yeast", 
                          "Sprouts and shoots", "Supplements", 
                          "Gelling and binding agents", "Sugar and sweeteners", 
                          "Fats", "Flakes, bran and germs", "Herbs", "Salad dressings", 
                          "Salt, spices and flavours", "Oils", "Sauces", 
                          "Flour and starch", "Dough", "Spreads", "Fish", "Vegetables", 
                          "Beverages based on malt extract", "Oriental dishes", 
                          "Soft drinks energy reduced", "Drinking water", 
                          "Fish products", "Water based ice cream", 
                          "Salt sticks and pretzels", "Other alcoholic beverages", 
                          "Tea", "Mayonnaise", "Cordial")) %>% 
  droplevels() %>% 
  mutate(Category = 
           fct_collapse(Category, 
                        "Other sweet dishes" = "Milk based ice cream", 
                        "Fruit and vegetable juices" = c("Fruit juices", 
                                                         "Vegetable juices"), 
                        "Chocolate and cocoa products" = "Cocoa beverages", 
                        "Cooked sausage products" = "Other sausages and cold meats", 
                        "Other cereal products" = "Bars", 
                        "Dried fruit and vegetables" = c("Dried fruit", "Dried vegetables"), 
                        "Pulses" = "Corn", "Milk" = "Milk and yoghurt beverages", 
                        "Nuts, seeds and oleaginous fruit" = 
                          "Salted nuts, seeds and kernels")) %>% 
  mutate(MainCategory = 
           fct_collapse(Category, 
                        "Beverages" = 
                          c("Beer", "Coffee", "Fruit and vegetable juices", 
                            "Soft drinks", "Spirits", "Wine"),
                        "Cakes, sweets and snacks" = 
                          c("Biscuits", "Cakes and tarts", "Cakes, pies and gratins", 
                            "Candies, fruits gums and chewing gum", 
                            "Chocolate and cocoa products", "Creams and puddings", 
                            "Crispbreads, rusks, crackers and wafers", "Crisps", 
                            "Jams and sweet sandwich spreads", "Other savoury snacks", 
                            "Other sweet pastries", "Other sweets"),
                        "Dairy products" = 
                          c("Cheese products", "Cream and butter", "Fresh cheese and curds", 
                            "Hard cheese", "Milk", "Milk substitutes", "Soft cheese", 
                            "Yogurt and curdled milk"),
                        "Dishes and fast food" = 
                          c("Asian dishes", "Fast Food", "Italian dishes", 
                            "Sandwiches", "Stews and soups", "Other savoury dishes", 
                            "Other sweet dishes"),
                        "Fruit and vegetables" = 
                          c("Cooked fruit (incl. cans)", 
                            "Cooked vegetables (incl. cans)", 
                            "Dried fruit and vegetables", "Fresh fruit", 
                            "Fresh vegetables", "Mushrooms", 
                            "Nuts, seeds and oleaginous fruit", "Pulses", "Salads"), 
                        "Grains and potatoes" = 
                          c("Bread and bread products", "Muesli and pudding", 
                            "Muesli mixes and breakfast cereals", 
                            "Other cereal products", "Pasta", "Potatoes", "Rice"), 
                        "Meat, fish and eggs" = 
                          c("Beef", "Boiled sausage products", "Cooked sausage products", 
                            "Eggs", "Fresh water fish", "Game", "Lamb, mutton", 
                            "Meat and offal", "Meat substitutes", "Pork", "Poultry", 
                            "Raw sausage products", "Sea fish", 
                            "Seafood, crustaceans and shellfish", "Veal", 
                            "Other animal species")))

# Neu anordnen der Factor Levels für Variable "MainCategory"--------------------

food_02$MainCategory <- 
  factor(x = food_02$MainCategory, 
         levels = c("Beverages", "Cakes, sweets and snacks", "Dairy products", 
                    "Dishes and fast food", "Fruit and vegetables", 
                    "Grains and potatoes", "Meat, fish and eggs"))

# "Category" in Variablen speichern & Levels neu anordnen ----------------------

varBeverage <- filter(food_02, MainCategory == "Beverages")
varBeverage$Category <- 
  factor(x = varBeverage$Category, 
         levels = c("Beer", "Coffee", "Fruit and vegetable juices", "Soft drinks", 
                    "Spirits", "Wine"))

varCake <- filter(food_02, MainCategory == "Cakes, sweets and snacks")
varCake$Category <- 
  factor(x = varCake$Category, 
         levels = c("Biscuits", "Cakes and tarts", "Cakes, pies and gratins", 
                    "Candies, fruits gums and chewing gum", "Chocolate and cocoa products", 
                    "Creams and puddings", "Crispbreads, rusks, crackers and wafers", 
                    "Crisps", "Jams and sweet sandwich spreads", "Other savoury snacks", 
                    "Other sweet pastries", "Other sweets"))

varDish <- filter(food_02, MainCategory == "Dishes and fast food")
varDish$Category <- 
  factor(x = varDish$Category, 
         levels = c("Asian dishes", "Fast Food", "Italian dishes", "Sandwiches", 
                    "Stews and soups", "Other savoury dishes", "Other sweet dishes"))

varFV <- filter(food_02, MainCategory == "Fruit and vegetables")
varFV$Category <- 
  factor(x = varFV$Category, 
         levels = c("Cooked fruit (incl. cans)", "Cooked vegetables (incl. cans)", 
                    "Dried fruit and vegetables", "Fresh fruit", "Fresh vegetables", 
                    "Mushrooms", "Nuts, seeds and oleaginous fruit", "Pulses", "Salads"))

varGrain <- filter(food_02, MainCategory == "Grains and potatoes")
varGrain$Category <- 
  factor(x = varGrain$Category, 
         levels = c("Bread and bread products", "Muesli and pudding", 
                    "Muesli mixes and breakfast cereals", "Pasta", "Potatoes", 
                    "Rice", "Other cereal products"))

varMeat <- filter(food_02, MainCategory == "Meat, fish and eggs")
varMeat$Category <- 
  factor(x = varMeat$Category, 
         levels = c("Beef", "Boiled sausage products", "Cooked sausage products", 
                    "Eggs", "Fresh water fish", "Game", "Lamb, mutton", 
                    "Meat and offal", "Meat substitutes", "Pork", "Poultry", 
                    "Raw sausage products", "Sea fish", 
                    "Seafood, crustaceans and shellfish", "Veal", 
                    "Other animal species"))

varMilk <- filter(food_02, MainCategory == "Dairy products")
varMilk$Category <- 
  factor(x = varMilk$Category, 
         levels = c("Cheese products", "Cream and butter", "Fresh cheese and curds", 
                    "Hard cheese", "Milk", "Milk substitutes", "Soft cheese", 
                    "Yogurt and curdled milk"))

# Werte für Variable selX & selY speichern -------------------------------------

varFeat <- names(food_02)
varFeat <- 
  varFeat[!varFeat %in% c("ID", "Name", "Category", "Folate (µg)", "MainCategory")]

# Datensatz für Tabelle erstellen ----------------------------------------------
# nicht mehr notwendig

# food_03 <- food_02 %>% 
#   select(!c("ID", "Category", "Fatty acids (g)", "Cholesterol (mg)", 
#             "Dietary fibres (g)", "Folate (µg)", "Vitamin C (mg)", "Calcium (mg)", 
#             "Magnesium (mg)", "Iron (mg)", "MainCategory"))
