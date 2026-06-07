#!/usr/bin/env Rscript
# =============================================================================
#  forecast.R  —  daily World Cup 2026 forecast engine (runs in GitHub Actions)
#
#  Reads played results from data/results.csv, applies the same live-update
#  logic as the web app (fix played scorelines + nudge Elo with K=40), runs the
#  Elo -> Poisson -> Monte Carlo simulation, and writes:
#     data/forecast.json   (consumed by index.html)
#     FORECAST.md          (renders as a table on the repo home page)
#
#  Base R only — no packages, so the CI job needs no install step.
#  Tune sims with the N_SIMS environment variable (default 10000).
# =============================================================================

set.seed(as.integer(Sys.getenv("SEED", "2026")))
N_SIMS <- as.integer(Sys.getenv("N_SIMS", "10000"))

# ---- 1. teams (canonical ASCII names; must match fetch_results.R output) -----
teams <- read.csv(text = "
team,group,elo
Mexico,A,1800
South Africa,A,1640
Korea Republic,A,1755
Czechia,A,1790
Canada,B,1735
Bosnia and Herzegovina,B,1705
Qatar,B,1680
Switzerland,B,1897
Brazil,C,1988
Morocco,C,1860
Haiti,C,1530
Scotland,C,1775
United States,D,1795
Paraguay,D,1760
Australia,D,1715
Turkiye,D,1880
Germany,E,1910
Curacao,E,1535
Ivory Coast,E,1810
Ecuador,E,1933
Netherlands,F,1959
Japan,F,1879
Sweden,F,1775
Tunisia,F,1690
Belgium,G,1849
Egypt,G,1700
Iran,G,1785
New Zealand,G,1500
Spain,H,2155
Cape Verde,H,1620
Saudi Arabia,H,1640
Uruguay,H,1890
France,I,2062
Senegal,I,1869
Iraq,I,1620
Norway,I,1922
Argentina,J,2113
Algeria,J,1745
Austria,J,1790
Jordan,J,1620
Portugal,K,1984
DR Congo,K,1720
Uzbekistan,K,1690
Colombia,K,1977
England,L,2020
Croatia,L,1933
Ghana,L,1700
Panama,L,1675
", strip.white = TRUE, stringsAsFactors = FALSE)

elo   <- setNames(teams$elo, teams$team)
grp   <- setNames(teams$group, teams$team)
hosts <- c("Mexico", "Canada", "United States")
group_letters <- LETTERS[1:12]

# ---- 2. results: read + apply (lock scorelines, nudge Elo) -------------------
results_path <- "data/results.csv"
results <- data.frame(home = character(), away = character(),
                      home_score = integer(), away_score = integer(),
                      stringsAsFactors = FALSE)
if (file.exists(results_path)) {
  r <- read.csv(results_path, stringsAsFactors = FALSE, strip.white = TRUE)
  r <- r[!is.na(r$home_score) & !is.na(r$away_score) &
         r$home %in% teams$team & r$away %in% teams$team, , drop = FALSE]
  if (nrow(r)) results <- r
}
n_results <- nrow(results)

# Elo nudge from each played result (same rule as the app: K=40, GD multiplier)
for (i in seq_len(n_results)) {
  h <- results$home[i]; a <- results$away[i]
  hs <- results$home_score[i]; as_ <- results$away_score[i]
  E  <- 1 / (1 + 10 ^ (-(elo[[h]] - elo[[a]]) / 400))
  S  <- if (hs > as_) 1 else if (hs < as_) 0 else 0.5
  gd <- abs(hs - as_)
  gmul  <- if (gd <= 1) 1 else if (gd == 2) 1.5 else (11 + gd) / 8
  delta <- 40 * gmul * (S - E)
  elo[[h]] <- elo[[h]] + delta
  elo[[a]] <- elo[[a]] - delta
}

lookup_result <- function(a, b) {
  i <- which(results$home == a & results$away == b)
  if (length(i)) return(c(results$home_score[i[1]], results$away_score[i[1]]))
  j <- which(results$home == b & results$away == a)
  if (length(j)) return(c(results$away_score[j[1]], results$home_score[j[1]]))
  NULL
}

# ---- 3. match model ----------------------------------------------------------
host_bump <- function(team, b) if (team %in% hosts) b else 0
lambdas <- function(a, b, ko) {
  bump <- if (ko) 30 else 55
  Rd   <- elo[[a]] - elo[[b]] + host_bump(a, bump) - host_bump(b, bump)
  sup  <- Rd / 250
  c(lamA = max(0.18, 1.38 + sup/2), lamB = max(0.18, 1.38 - sup/2), Rd = Rd)
}
play_ko <- function(a, b) {
  L <- lambdas(a, b, TRUE)
  ga <- rpois(1, L[["lamA"]]); gb <- rpois(1, L[["lamB"]])
  if (ga > gb) return(a); if (gb > ga) return(b)
  if (runif(1) < 1/(1 + 10^(-L[["Rd"]]/400))) a else b
}

# ---- 4. bracket --------------------------------------------------------------
spec <- function(t, g = NA) list(t = t, g = g)
R32 <- list(
  list(spec("R","A"),spec("R","B")), list(spec("W","C"),spec("R","F")),
  list(spec("W","E"),spec("T")),     list(spec("W","F"),spec("R","C")),
  list(spec("R","E"),spec("R","I")), list(spec("W","I"),spec("T")),
  list(spec("W","A"),spec("T")),     list(spec("W","L"),spec("T")),
  list(spec("W","G"),spec("T")),     list(spec("W","D"),spec("T")),
  list(spec("W","H"),spec("R","J")), list(spec("R","K"),spec("R","L")),
  list(spec("W","B"),spec("T")),     list(spec("R","D"),spec("R","G")),
  list(spec("W","J"),spec("R","H")), list(spec("W","K"),spec("T")))
third_slots <- list(
  list(pos=3, el=c("A","B","C","D","F")), list(pos=6, el=c("C","D","F","G","H")),
  list(pos=7, el=c("C","E","F","H","I")), list(pos=8, el=c("E","H","I","J","K")),
  list(pos=9, el=c("A","E","H","I","J")), list(pos=10,el=c("B","E","F","I","J")),
  list(pos=13,el=c("E","F","G","I","J")), list(pos=16,el=c("D","E","I","J","L")))
R16_pairs <- list(c(11,1),c(4,10),c(6,5),c(2,7),c(15,14),c(3,12),c(8,13),c(9,16))
QF_pairs  <- list(c(1,2),c(3,4),c(5,6),c(7,8))
SF_pairs  <- list(c(1,2),c(3,4))

assign_thirds <- function(third_groups) {
  used <- logical(length(third_slots)); out <- character(length(third_slots))
  bt <- function(i) {
    if (i > length(third_groups)) return(TRUE)
    g <- third_groups[i]
    for (s in seq_along(third_slots)) if (!used[s] && g %in% third_slots[[s]]$el) {
      used[s] <<- TRUE; out[s] <<- g
      if (bt(i + 1)) return(TRUE)
      used[s] <<- FALSE; out[s] <<- ""
    }
    FALSE
  }
  bt(1)
  setNames(out, vapply(third_slots, function(s) s$pos, numeric(1)))
}

# ---- 5. one tournament -------------------------------------------------------
simulate_group <- function(gteams) {
  pts <- gd <- gf <- setNames(numeric(length(gteams)), gteams)
  combos <- combn(gteams, 2)
  for (k in seq_len(ncol(combos))) {
    a <- combos[1,k]; b <- combos[2,k]
    sc <- lookup_result(a, b)                      # played?  use real score
    if (is.null(sc)) {                             # else simulate
      L <- lambdas(a, b, FALSE)
      sc <- c(rpois(1, L[["lamA"]]), rpois(1, L[["lamB"]]))
    }
    ga <- sc[1]; gb <- sc[2]
    gf[a] <- gf[a]+ga; gf[b] <- gf[b]+gb
    gd[a] <- gd[a]+ga-gb; gd[b] <- gd[b]+gb-ga
    if (ga > gb) pts[a] <- pts[a]+3 else if (gb > ga) pts[b] <- pts[b]+3
    else { pts[a] <- pts[a]+1; pts[b] <- pts[b]+1 }
  }
  ord <- order(-pts, -gd, -gf, runif(length(gteams)))
  ranked <- gteams[ord]
  list(winner = ranked[1], runner = ranked[2], third = ranked[3],
       third_stats = c(pts[ranked[3]], gd[ranked[3]], gf[ranked[3]]))
}

simulate_tournament <- function() {
  winners <- runners <- thirds <- setNames(character(12), group_letters)
  pool <- data.frame(g = group_letters, pts = 0, gd = 0, gf = 0)
  for (g in group_letters) {
    res <- simulate_group(names(grp[grp == g]))
    winners[g] <- res$winner; runners[g] <- res$runner; thirds[g] <- res$third
    pool[pool$g == g, c("pts","gd","gf")] <- res$third_stats
  }
  best8 <- pool[order(-pool$pts, -pool$gd, -pool$gf, runif(12)), ][1:8, "g"]
  tassign <- assign_thirds(best8)
  resolve <- function(sp, pos) {
    if (sp$t == "W") return(winners[[sp$g]])
    if (sp$t == "R") return(runners[[sp$g]])
    thirds[[ tassign[[as.character(pos)]] ]]
  }
  r32 <- lapply(seq_along(R32), function(p)
    c(resolve(R32[[p]][[1]], p), resolve(R32[[p]][[2]], p)))
  r16 <- vapply(r32, function(m) play_ko(m[1], m[2]), character(1))
  qf  <- vapply(R16_pairs, function(p) play_ko(r16[p[1]], r16[p[2]]), character(1))
  sf  <- vapply(QF_pairs,  function(p) play_ko(qf[p[1]],  qf[p[2]]),  character(1))
  fin <- vapply(SF_pairs,  function(p) play_ko(sf[p[1]],  sf[p[2]]),  character(1))
  list(champion = play_ko(fin[1], fin[2]), finalist = fin, semifinal = sf,
       quarterfinal = qf, last16 = r16, last32 = unlist(r32),
       group_winner = unname(winners))
}

# ---- 6. monte carlo ----------------------------------------------------------
zero <- function() setNames(numeric(nrow(teams)), teams$team)
acc <- list(champion=zero(), finalist=zero(), semifinal=zero(),
            quarterfinal=zero(), last32=zero(), group_winner=zero())
cat(sprintf("Running %d simulations  (%d results locked)\n", N_SIMS, n_results))
for (i in seq_len(N_SIMS)) {
  s <- simulate_tournament()
  acc$champion[s$champion]         <- acc$champion[s$champion] + 1
  acc$finalist[s$finalist]         <- acc$finalist[s$finalist] + 1
  acc$semifinal[s$semifinal]       <- acc$semifinal[s$semifinal] + 1
  acc$quarterfinal[s$quarterfinal] <- acc$quarterfinal[s$quarterfinal] + 1
  acc$last32[s$last32]             <- acc$last32[s$last32] + 1
  acc$group_winner[s$group_winner] <- acc$group_winner[s$group_winner] + 1
}
df <- data.frame(
  team=teams$team, group=teams$group, elo=round(elo[teams$team]),
  champion=acc$champion/N_SIMS, finalist=acc$finalist/N_SIMS,
  semifinal=acc$semifinal/N_SIMS, quarterfinal=acc$quarterfinal/N_SIMS,
  last32=acc$last32/N_SIMS, win_group=acc$group_winner/N_SIMS, row.names=NULL)
df <- df[order(-df$champion, -df$finalist), ]

# ---- 7. write outputs --------------------------------------------------------
dir.create("data", showWarnings = FALSE)
updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# forecast.json (hand-rolled, zero deps)
esc <- function(s) gsub('"', '\\\\"', s)
rows <- character(nrow(df))
for (i in seq_len(nrow(df))) rows[i] <- sprintf(
  '    {"team":"%s","group":"%s","elo":%d,"champion":%.4f,"finalist":%.4f,"semifinal":%.4f,"quarterfinal":%.4f,"last32":%.4f,"win_group":%.4f}',
  esc(df$team[i]), df$group[i], as.integer(df$elo[i]),
  df$champion[i], df$finalist[i], df$semifinal[i],
  df$quarterfinal[i], df$last32[i], df$win_group[i])
json <- paste0('{\n  "updated": "', updated, '",\n  "n_sims": ', N_SIMS,
               ',\n  "results_used": ', n_results,
               ',\n  "teams": [\n', paste(rows, collapse = ",\n"), '\n  ]\n}\n')
writeLines(json, "data/forecast.json")

# FORECAST.md (top 12, renders on the repo page)
pct <- function(x) sprintf("%.1f%%", x * 100)
md <- c(
  "# Mundial '26 — live forecast",
  sprintf("_Updated %s · %s simulations · %d results in_", updated, format(N_SIMS, big.mark=","), n_results),
  "",
  "| # | Team | Grp | Champion | Final | Semi | Win group |",
  "|---|------|-----|----------|-------|------|-----------|")
top <- head(df, 12)
for (i in seq_len(nrow(top))) md <- c(md, sprintf(
  "| %d | %s | %s | %s | %s | %s | %s |", i, top$team[i], top$group[i],
  pct(top$champion[i]), pct(top$finalist[i]), pct(top$semifinal[i]), pct(top$win_group[i])))
writeLines(md, "FORECAST.md")

cat("Wrote data/forecast.json and FORECAST.md\n")
cat(sprintf("Leader: %s (%s)\n", df$team[1], pct(df$champion[1])))
