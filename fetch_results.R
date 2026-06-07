#!/usr/bin/env Rscript
# =============================================================================
#  fetch_results.R  —  pull the latest 2026 World Cup finals results and write
#  data/results.csv with our canonical team names. Base R only.
#
#  Source: martj42/international_results (the same open dataset used to build
#  the historical Octopus heatmap). No API key required. It is community-
#  maintained and typically updated within a day of each match.
#
#  In CI the source CSV is downloaded to /tmp/all.csv first; override with the
#  SRC env var when testing locally.
# =============================================================================

src <- Sys.getenv("SRC", "/tmp/all.csv")
if (!file.exists(src)) { message("source not found: ", src); quit(status = 0) }
invisible(suppressWarnings(try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE)))

all <- tryCatch(read.csv(src, stringsAsFactors = FALSE, encoding = "UTF-8"),
                error = function(e) { message("read failed: ", conditionMessage(e)); quit(status = 0) })

# keep only World Cup *finals* matches in 2026 that have actually been played
wc <- all[all$tournament == "FIFA World Cup" & substr(all$date, 1, 4) == "2026", , drop = FALSE]
wc <- wc[!is.na(wc$home_score) & !is.na(wc$away_score), , drop = FALSE]

# ---- name normalisation: source -> our canonical names ----------------------
canonical <- c("Mexico","South Africa","Korea Republic","Czechia","Canada",
  "Bosnia and Herzegovina","Qatar","Switzerland","Brazil","Morocco","Haiti",
  "Scotland","United States","Paraguay","Australia","Turkiye","Germany",
  "Curacao","Ivory Coast","Ecuador","Netherlands","Japan","Sweden","Tunisia",
  "Belgium","Egypt","Iran","New Zealand","Spain","Cape Verde","Saudi Arabia",
  "Uruguay","France","Senegal","Iraq","Norway","Argentina","Algeria","Austria",
  "Jordan","Portugal","DR Congo","Uzbekistan","Colombia","England","Croatia",
  "Ghana","Panama")
alias <- c(
  "South Korea"="Korea Republic", "Korea Republic"="Korea Republic",
  "Turkey"="Turkiye", "Turkiye"="Turkiye",
  "Czech Republic"="Czechia", "Czechia"="Czechia",
  "Cabo Verde"="Cape Verde", "Cape Verde"="Cape Verde",
  "Cote d'Ivoire"="Ivory Coast", "Ivory Coast"="Ivory Coast",
  "Congo DR"="DR Congo", "DR Congo"="DR Congo",
  "Democratic Republic of the Congo"="DR Congo",
  "Curacao"="Curacao", "USA"="United States", "United States"="United States",
  "IR Iran"="Iran")
# locale-proof accent stripping: handles both real UTF-8 codepoints (normal
# runners) and R's "<U+00E7>" escape form (single-byte locales).
deaccent <- function(s) {
  esc <- c("00C0","00C1","00C2","00C3","00C4","00C7","00C8","00C9","00CA","00CB",
           "00CC","00CD","00CE","00CF","00D1","00D2","00D3","00D4","00D5","00D6",
           "00D9","00DA","00DB","00DC","00DD","00E0","00E1","00E2","00E3","00E4",
           "00E7","00E8","00E9","00EA","00EB","00EC","00ED","00EE","00EF","00F1",
           "00F2","00F3","00F4","00F5","00F6","00F9","00FA","00FB","00FC","00FD")
  base <- c("A","A","A","A","A","C","E","E","E","E","I","I","I","I","N","O","O","O",
            "O","O","U","U","U","U","Y","a","a","a","a","a","c","e","e","e","e","i",
            "i","i","i","n","o","o","o","o","o","u","u","u","u","y")
  for (k in seq_along(esc)) s <- gsub(paste0("<U\\+", esc[k], ">"), base[k], s, ignore.case = TRUE)
  oc <- intToUtf8(strtoi(esc, 16L), multiple = TRUE)
  chartr(paste(oc, collapse = ""), paste(base, collapse = ""), s)
}
to_canon <- function(name) {
  n <- deaccent(name)
  if (!is.na(alias[n])) return(unname(alias[n]))
  n2 <- gsub("[`'^~\"]", "", n)              # drop stray quotes (e.g. d'Ivoire)
  if (!is.na(alias[n2])) return(unname(alias[n2]))
  if (n %in% canonical) return(n)
  NA_character_
}

out <- data.frame(home = character(), away = character(),
                  home_score = integer(), away_score = integer(),
                  stringsAsFactors = FALSE)
dropped <- 0
for (i in seq_len(nrow(wc))) {
  h <- to_canon(wc$home_team[i]); a <- to_canon(wc$away_team[i])
  if (is.na(h) || is.na(a)) { dropped <- dropped + 1; next }
  out <- rbind(out, data.frame(home = h, away = a,
    home_score = as.integer(wc$home_score[i]),
    away_score = as.integer(wc$away_score[i]), stringsAsFactors = FALSE))
}

dir.create("data", showWarnings = FALSE)
write.csv(out, "data/results.csv", row.names = FALSE)
cat(sprintf("Wrote data/results.csv: %d WC 2026 results (%d unmapped rows skipped)\n",
            nrow(out), dropped))
