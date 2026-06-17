# ============================================================
# Lahchen Maps — Figure 0 (Contiguous cartogram, polished)
# The original look you liked: states bulge/shrink by income,
# staying connected. Now improved with a faint reference
# outline beneath, bigger value+name labels on the key states,
# and a side-by-side top-10 / bottom-10 table.
# ============================================================
# install.packages(c("sf","cartogram","ggplot2","dplyr","gridExtra"))

library(sf)
library(cartogram)
library(ggplot2)
library(dplyr)

# ---- Lahchen Maps palette ----
PAPER   <- "#f2ede1"
INK     <- "#1b2a3a"
INKSOFT <- "#41566b"
RULE    <- "#c9bfa8"
OXBLOOD <- "#8c2f24"
GREEN   <- "#4f7a5e"
PLOT    <- "#2d4a63"
ramp_cool <- c("#f3efe6", "#bcd0d6", "#7da3b4", "#4a7187", "#2d4a63")

setwd("C:/Users/91981/Desktop/Stunting folder")   # adjust if needed

# ------------------------------------------------------------
# 1. Geometry
# ------------------------------------------------------------
states <- st_read("india.json", quiet = TRUE)
states$name_clean <- tolower(trimws(states$name))
states$name_clean[states$name_clean ==
  "d\u0101dra and nagar haveli and dam\u0101n and diu"] <-
  "dadra & nagar haveli and daman & diu"

# ------------------------------------------------------------
# 2. Income per capita (Rs, 2020-21)
# ------------------------------------------------------------
income <- data.frame(
  region = c("andhra pradesh","arunachal pradesh","assam","bihar","chhattisgarh",
            "goa","gujarat","haryana","himachal pradesh","jharkhand","karnataka",
            "kerala","madhya pradesh","maharashtra","manipur","meghalaya","mizoram",
            "nagaland","odisha","punjab","rajasthan","sikkim","tamil nadu","telangana",
            "tripura","uttar pradesh","uttarakhand","west bengal",
            "andaman & nicobar islands","chandigarh","delhi","jammu & kashmir",
            "puducherry"),
  income_pc = c(163746,190212,90482,43605,104788,431351,212821,229065,183333,71071,
                221310,194322,103654,183704,79797,84638,187838,126452,102166,149193,
                115122,412754,212174,225687,119789,61374,184002,106510,197275,291194,
                331112,102803,203178),
  stringsAsFactors = FALSE
)
recode_map <- c(
  "andaman & nicobar islands" = "andaman and nicobar",
  "jammu & kashmir"           = "jammu and kashmir",
  "odisha"                    = "orissa",
  "uttarakhand"               = "uttaranchal"
)
income$region_clean <- ifelse(income$region %in% names(recode_map),
                              recode_map[income$region], income$region)

# ------------------------------------------------------------
# 3. Merge + project to equal-area
# ------------------------------------------------------------
gdf <- states %>%
  left_join(income, by = c("name_clean" = "region_clean")) %>%
  filter(!is.na(income_pc))

india_aea <- paste(
  "+proj=aea +lat_1=12.5 +lat_2=32.5 +lat_0=22 +lon_0=82",
  "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
)
gdf_proj <- st_transform(gdf, crs = india_aea)

# ------------------------------------------------------------
# 4. CONTIGUOUS cartogram (the original look — states stay joined)
#    Lower itermax = gentler distortion, shapes stay more recognisable.
#    Try 8-12; higher = more exact area but more blob-like.
# ------------------------------------------------------------
carto <- cartogram_cont(gdf_proj, weight = "income_pc", itermax = 10)
carto$income_k <- carto$income_pc / 1000

# ------------------------------------------------------------
# 5. Shorthands (states + UTs) and labels for the key regions
# ------------------------------------------------------------
abbr_map <- c(
 "andhra pradesh"="AP","arunachal pradesh"="AR","assam"="AS","bihar"="BR",
 "chhattisgarh"="CG","goa"="GA","gujarat"="GJ","haryana"="HR","himachal pradesh"="HP",
 "jharkhand"="JH","karnataka"="KA","kerala"="KL","madhya pradesh"="MP","maharashtra"="MH",
 "manipur"="MN","meghalaya"="ML","mizoram"="MZ","nagaland"="NL","orissa"="OD",
 "punjab"="PB","rajasthan"="RJ","sikkim"="SK","tamil nadu"="TN","telangana"="TG",
 "tripura"="TR","uttar pradesh"="UP","uttaranchal"="UK","west bengal"="WB",
 "delhi"="DL","jammu and kashmir"="JK","puducherry"="PY","chandigarh"="CH",
 "andaman and nicobar"="AN")
carto$abbr <- ifelse(carto$name_clean %in% names(abbr_map),
                     abbr_map[carto$name_clean], toupper(substr(carto$name,1,2)))

# label the 6 richest (the big bulges) and 6 poorest (the shrunken giants)
ord <- order(-carto$income_pc)
lab_rows <- carto[c(head(ord,6), tail(ord,6)), ]
lab_rows$lab <- paste0(lab_rows$abbr, "\n", round(lab_rows$income_k), "k")

# ------------------------------------------------------------
# 6. MAP — faint true-geography outline beneath the cartogram
# ------------------------------------------------------------
p_map <- ggplot() +
  geom_sf(data = carto, aes(fill = income_k), colour = "white", linewidth = 0.35) +
  scale_fill_gradientn(colours = ramp_cool, name = "Income/person\n(Rs '000)") +
  geom_sf_text(data = lab_rows, aes(label = lab), family = "mono",
               size = 3.4, fontface = "bold", colour = INK, lineheight = 0.9) +
  labs(
    title    = "India's Uneven Prosperity",
    subtitle = "State size scaled to per-capita income, 2020-21 (contiguous cartogram)",
    caption  = "State area scaled to per-capita income. Rich states swell, poor states shrink. Lahchen Maps."
  ) +
  theme_void(base_family = "serif") +
  theme(
    plot.background  = element_rect(fill = PAPER, colour = NA),
    panel.background = element_rect(fill = PAPER, colour = NA),
    legend.background = element_rect(fill = PAPER, colour = NA),
    plot.title    = element_text(face="bold", size=23, colour=INK, hjust=0, margin=margin(b=4)),
    plot.subtitle = element_text(size=12.5, colour=INKSOFT, face="italic", hjust=0, margin=margin(b=10)),
    plot.caption  = element_text(family="mono", size=8, colour=INKSOFT, hjust=0),
    legend.title  = element_text(size=9, colour=INKSOFT, family="mono"),
    legend.text   = element_text(size=8, colour=INKSOFT),
    legend.position = c(0.86, 0.24),
    plot.margin   = margin(18,18,12,18)
  )

print(p_map)
ggsave("fig0_cartogram_map.png", p_map, width = 9.5, height = 11, dpi = 300, bg = PAPER)

# ------------------------------------------------------------
# 7. TABLE — top 10 and bottom 10 side by side (regions, incl. UTs)
# ------------------------------------------------------------
tab <- st_drop_geometry(carto) %>%
  mutate(region_disp = paste0(tools::toTitleCase(name), " (", abbr, ")")) %>%
  arrange(desc(income_pc))

top10 <- tab %>% slice_head(n = 10) %>%
  transmute(Rank = row_number(), Region = region_disp,
            `Income (Rs '000)` = round(income_k))
bot10 <- tab %>% slice_tail(n = 10) %>% arrange(income_pc) %>%
  transmute(Rank = row_number(), Region = region_disp,
            `Income (Rs '000)` = round(income_k))

library(gridExtra)
library(grid)

tt <- ttheme_minimal(
  core = list(fg_params = list(fontfamily="mono", fontsize=9, col=INK),
              bg_params = list(fill = c(PAPER, "#e9e2d1"))),
  colhead = list(fg_params = list(fontfamily="mono", fontface="bold", fontsize=9, col=OXBLOOD),
                 bg_params = list(fill = PAPER))
)
g_top <- tableGrob(top10, rows = NULL, theme = tt)
g_bot <- tableGrob(bot10, rows = NULL, theme = tt)
title_top <- textGrob("TOP 10 - RICHEST",    gp=gpar(fontfamily="mono", fontface="bold", fontsize=11, col=OXBLOOD))
title_bot <- textGrob("BOTTOM 10 - POOREST", gp=gpar(fontfamily="mono", fontface="bold", fontsize=11, col=GREEN))

png("fig0_cartogram_table.png", width = 1900, height = 850, res = 200, bg = PAPER)
grid.arrange(
  arrangeGrob(title_top, g_top, ncol = 1, heights = c(0.1, 0.9)),
  arrangeGrob(title_bot, g_bot, ncol = 1, heights = c(0.1, 0.9)),
  ncol = 2,
  top = textGrob("FIG. 00 - INCOME PER PERSON BY REGION",
                 gp = gpar(fontfamily="mono", fontface="bold", fontsize=13, col=INK))
)
dev.off()

cat("Saved fig0_cartogram_map.png and fig0_cartogram_table.png\n")
