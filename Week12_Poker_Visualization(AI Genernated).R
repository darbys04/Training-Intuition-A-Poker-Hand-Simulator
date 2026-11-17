# Week 12 - Poker Probability Visualization
# Base R only. No additional packages and no need to re-run the simulations.
# Input: the six simulated counts from the HTML report (2026-11-17)
#        and the exact combinatorial counts from the theory report.
# Output: six conditional comparison PNG charts, one Excel trend PNG,
#         two unconditional 7-card distribution PNG charts,
#         one unconditional CSV table, and a combined comparison CSV.
# Unconditional 7-card counts use the exact number of distinct 7-card draws,
# and all categories use the BEST 5-card hand within those 7 cards.


# 1. Hand categories are kept in the same order as the Testing report.
types <- c("Royal Flush", "Straight Flush", "Four of a Kind", "Full House",
           "Flush", "Straight", "Three of a Kind", "Two Pair",
           "One Pair", "High Card")

case_names <- c("Case 1: A-Heart + K-Spade",
                "Case 2: Pair of Twos",
                "Case 3: 3C 4H 7S QH 9H",
                "Case 4: 10C JC QC KC 3D",
                "Case 5: 4D 5C 6S 7D 10H 10S",
                "Case 6: 3D 3C 6C 6D 10C JC")

# Each row below corresponds to the same ten hand types listed above.
# Exact counts are from all possible draws, not from another simulation.
exact_counts <- list(
  c(94, 122, 2668, 47124, 41562, 69954, 93808, 480080, 965568, 417780),
  c(4, 212, 17848, 181104, 41562, 25816, 249458, 840456, 762300, 0),
  c(0, 0, 0, 0, 45, 15, 15, 89, 512, 405),
  c(46, 45, 0, 0, 287, 207, 13, 78, 270, 135),
  c(0, 0, 0, 0, 0, 8, 2, 12, 24, 0),
  c(0, 0, 0, 4, 9, 0, 0, 33, 0, 0)
)

# Simulated counts are copied from the uploaded HTML report.
sim_counts <- list(
  c(0, 1, 7, 96, 121, 144, 205, 1092, 2307, 1027),
  c(0, 0, 45, 422, 100, 78, 589, 2008, 1758, 0),
  c(0, 0, 0, 0, 41, 20, 13, 90, 477, 359),
  c(45, 41, 0, 0, 245, 177, 12, 82, 269, 129),
  c(0, 0, 0, 0, 0, 93, 21, 150, 236, 0),
  c(0, 0, 0, 27, 91, 0, 0, 382, 0, 0)
)

exact_totals <- c(2118760, 2118760, 1081, 1081, 46, 46)
sim_totals <- c(5000, 5000, 1000, 1000, 500, 500)

# 7-card unconditional benchmark: no fixed cards, draw 7 cards uniformly.
# Categories are mutually exclusive; Royal Flush is kept separate
# from other Straight Flushes, matching the ten categories in the tests.
# Counts total choose(52, 7) = 133,784,560 combinations.
unconditional_7_counts <- c(
  4324,      # Royal Flush
  37260,     # Straight Flush (excluding Royal Flush)
  224848,    # Four of a Kind
  3473184,   # Full House
  4047644,   # Flush
  6180020,   # Straight
  6461620,   # Three of a Kind
  31433400,  # Two Pair
  58627800,  # One Pair
  23294460   # High Card
)
unconditional_7_total <- choose(52, 7)
stopifnot(sum(unconditional_7_counts) == unconditional_7_total)
unconditional_7_percent <- unconditional_7_counts / unconditional_7_total * 100
unconditional_7_data <- data.frame(
  Hand_Type = types,
  Exact_Count = unconditional_7_counts,
  Probability_Percent = round(unconditional_7_percent, 8)
)

# Basic checks prevent plotting mismatched data.
for (i in 1:6) {
  stopifnot(length(exact_counts[[i]]) == 10,
            length(sim_counts[[i]]) == 10,
            sum(exact_counts[[i]]) == exact_totals[i],
            sum(sim_counts[[i]]) == sim_totals[i])
}

# 2. Make a folder for all output files.
output_dir <- "Week12_Charts"
if (!dir.exists(output_dir)) dir.create(output_dir)

# 3. Plot one grouped horizontal bar chart per fixed-hand case.
all_results <- list()
for (i in 1:6) {
  theory <- exact_counts[[i]] / exact_totals[i] * 100
  simulation <- sim_counts[[i]] / sim_totals[i] * 100

  # Avoid ten crowded labels when some hand types are mathematically impossible.
  shown <- rev(which(theory > 0 | simulation > 0))
  bars <- rbind("Theory" = theory[shown], "Simulation" = simulation[shown])

  file_name <- file.path(output_dir, paste0("Case", i, "_Theory_vs_Simulation.png"))
  png(file_name, width = 1400, height = 900, res = 140)
  par(mar = c(5, 15, 5, 2))
  barplot(bars,
          beside = TRUE,
          horiz = TRUE,
          names.arg = types[shown],
          las = 1,
          col = c("steelblue", "orange"),
          xlab = "Probability (%)",
          main = paste0(case_names[i], "\nTheory vs Simulation"),
          xlim = c(0, max(bars) * 1.22))
  legend("topright", legend = c("Exact Theory", "Monte Carlo"),
         fill = c("steelblue", "orange"), bty = "n", cex = 0.85)
  dev.off()

  all_results[[i]] <- data.frame(
    Case = i,
    Hand_Type = types,
    Exact_Count = exact_counts[[i]],
    Theory_Percent = round(theory, 5),
    Simulation_Count = sim_counts[[i]],
    Simulation_Percent = round(simulation, 5),
    Difference_pp = round(simulation - theory, 5),
    Unconditional_7Card_Percent = round(unconditional_7_percent, 8)
  )
}

# 4. Create a CSV with every numerical comparison for the report.
comparison <- do.call(rbind, all_results)
write.csv(comparison,
          file.path(output_dir, "All_Cases_Theory_vs_Simulation.csv"),
          row.names = FALSE)

# Save the unconditional baseline as a separate table as well.
write.csv(unconditional_7_data,
          file.path(output_dir, "Unconditional_7Card_Distribution.csv"),
          row.names = FALSE)

# 5. Plot the unconditional best-five-from-seven distribution.
# The ordinary probability chart shows the dominant hand types.
png(file.path(output_dir, "Unconditional_7Card_Distribution.png"),
    width = 1500, height = 960, res = 150)
par(mar = c(5, 15, 5, 3))
plot_order <- 10:1
bar_centers <- barplot(unconditional_7_percent[plot_order],
    horiz = TRUE, names.arg = types[plot_order], las = 1,
    col = "steelblue", border = NA, xlim = c(0, 55),
    xlab = "Probability (%)",
    main = "Unconditional Poker Probability: Random 7 Cards")
text(unconditional_7_percent[plot_order] + 0.5, bar_centers,
     labels = sprintf("%.4f%%", unconditional_7_percent[plot_order]),
     pos = 4, cex = 0.8, xpd = FALSE)
dev.off()

# Zoom into all categories below 5% so rare events are visible.
rare_order <- rev(which(unconditional_7_percent < 5))
png(file.path(output_dir, "Unconditional_7Card_Rare_Hands.png"),
    width = 1500, height = 900, res = 150)
par(mar = c(5, 15, 5, 3))
rare_centers <- barplot(unconditional_7_percent[rare_order],
    horiz = TRUE, names.arg = types[rare_order], las = 1,
    col = "steelblue", border = NA, xlim = c(0, 5.7),
    xlab = "Probability (%)",
    main = "Random 7 Cards: Zoom on Rare Hand Types")
text(unconditional_7_percent[rare_order] + 0.06, rare_centers,
     labels = sprintf("%.4f%%", unconditional_7_percent[rare_order]),
     pos = 4, cex = 0.9, xpd = FALSE)
dev.off()


# 6. Trend plot using the corresponding data in Poker Probability.xlsx.
# Values below are percentages for random 5-, 6-, and 7-card pools.
card_numbers <- 5:7
high_card <- c(50.1177, 32.50, 17.41)
one_pair  <- c(42.2569, 47.80, 43.82)
two_pair  <- c(4.7539, 12.44, 23.50)

png(file.path(output_dir, "Excel_5_6_7_Card_Trend.png"),
    width = 1200, height = 780, res = 140)
par(mar = c(5, 5, 4, 2))
plot(card_numbers, high_card, type = "b", pch = 16,
     col = "steelblue", lwd = 3, xaxt = "n",
     ylim = c(0, 55), xlab = "Number of Cards",
     ylab = "Probability (%)",
     main = "Poker Probability by Number of Cards (Excel)")
axis(1, at = card_numbers)
lines(card_numbers, one_pair, type = "b", pch = 17, col = "orange", lwd = 3)
lines(card_numbers, two_pair, type = "b", pch = 15, col = "darkgreen", lwd = 3)
legend("topright", legend = c("High Card", "One Pair", "Two Pair"),
       col = c("steelblue", "orange", "darkgreen"),
       lty = 1, pch = c(16, 17, 15), bty = "n")
dev.off()

cat("Finished: 9 PNG charts + 2 CSV tables.\n")
cat("Output folder: ", normalizePath(output_dir), "\n", sep = "")
