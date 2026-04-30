%let pgm=utl-altair-slc-simulate-a-single-f-dq-channel-with-bit-pattern-test;

%stop_submission;

Altair slc simulate a single F-DQ channel with bit pattern test

Too long to post here, see github
[GITHUB](https://github.com/rogerjdeangelis/utl-altair-slc-simulate-a-single-f-dq-channel-with-bit-pattern-test)
https://github.com/rogerjdeangelis/utl-altair-slc-simulate-a-single-f-dq-channel-with-bit-pattern-test

Graphic output
[PLOT](https://github.com/rogerjdeangelis/utl-altair-slc-simulate-a-single-f-dq-channel-with-bit-pattern-test/blob/main/bit_test.pdf)
https://github.com/rogerjdeangelis/utl-altair-slc-simulate-a-single-f-dq-channel-with-bit-pattern-test/blob/main/bit_test.pdf

[SIEMENS FORUM](https://support.industry.siemens.com/cs/document/109801902)
https://support.industry.siemens.com/cs/document/109801902/

Conceptual setup
We model one F-DQ channel as a boolean output (True/False).

This is an oversiplified example but I think yo get the gist?

The bit pattern test” toggles this channel in a small sequence (e.g., [0,1,1,0]) once every
test period (simulating  interval tests seen in real F-DQ modules).

WHAT WE WANT

   Expected pattern: 1 0 1 0
   Actual pattern:   1 0 1 0

   VERIFIED: Pattern matches correctly
   NO FAULTS DETECTED - All test cycles passed

   = == == == == == == == == == == ==
   F-DQ CHANNEL SIMULATION RESULTS"
   = == == == == == == == == == == ==

                         actual    and
      time_step channel  pattern  test    in_test
   1          0       1     1       1       TRUE
   2          1       0     0       0       TRUE
   3          2       1     1       1       TRUE
   4          3       0     0       0       TRUE
   5          4       0     0       0      FALSE
   6          5       0     0       0      FALSE
   7          6       0     0       0      FALSE
   8          7       0     0       0      FALSE
   9          8       0     0       0      FALSE
  10          9       0     0       0      FALSE

/*
 _ __  _ __ ___   ___  ___ ___
| `_ \| `__/ _ \ / _ \/ __/ __|
| |_) | | | (_) |  __/\__ \__ \
| .__/|_|  \___/ \___||___/___/
|_|
*/

options validvarname=v7;
options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
proc r;
submit;
# Simple F-DQ Channel Simulation in R
# Simulates one channel with periodic bit-pattern test

# Load required libraries
library(ggplot2)
library(dplyr)

# Configuration
test_pattern <- c(1, 0, 1, 0)  # Bit-pattern test sequence (1=ON, 0=OFF)
test_period <- 10                # Run test every 10 time steps
pattern_length <- length(test_pattern)
n_steps <- 30                    # Total simulation steps

# Initialize vectors
channel <- numeric(n_steps)      # Channel output
in_test <- logical(n_steps)      # Test active flag
process_value <- numeric(n_steps) # Process demand (normally 0)

# Simulation
for (t in 1:n_steps) {
  # Check if we're at the start of a test period
  if ((t - 1) %% test_period == 0 && (t + pattern_length - 1) <= n_steps) {
    # Run bit-pattern test
    for (k in 1:pattern_length) {
      channel[t + k - 1] <- test_pattern[k]
      in_test[t + k - 1] <- TRUE
    }
    # Skip normal assignment for test duration
    next
  }

  # Normal operation (not in test)
  if (!in_test[t]) {
    channel[t] <- process_value[t]  # Follow process demand (0 = OFF)
  }
}

# Create dataframe for analysis
df <- data.frame(
  time_step = 0:(n_steps-1),
  channel = channel,
  in_test = in_test
)

# Print results
print("=" %>% paste(rep("=", 60), collapse=""))
print("F-DQ CHANNEL SIMULATION RESULTS")
print("=" %>% paste(rep("=", 60), collapse=""))
print(df)

# Summary statistics
cat("\n", "=" %>% paste(rep("=", 60), collapse=""), "\n")
cat("SUMMARY STATISTICS\n")
cat("=" %>% paste(rep("=", 60), collapse=""), "\n")
cat("Total steps:", n_steps, "\n")
cat("Test pattern:", test_pattern, "\n")
cat("Test period: every", test_period, "steps\n")
cat("Pattern length:", pattern_length, "steps\n")
cat("Total test cycles:", sum(df$in_test) / pattern_length, "\n")
cat("Steps in test mode:", sum(df$in_test), "\n")
cat("Steps in normal mode:", sum(!df$in_test), "\n")

# Find test cycles
test_starts <- which(df$in_test & !lag(df$in_test, default=FALSE))
cat("\nTest cycles start at time steps:", test_starts - 1, "\n")

# Verify pattern for first test cycle
cat("\n", "=" %>% paste(rep("=", 60), collapse=""), "\n")
cat("VERIFY FIRST TEST CYCLE\n")
cat("=" %>% paste(rep("=", 60), collapse=""), "\n")
first_test <- test_starts[1]
cat("Expected pattern:", test_pattern, "\n")
cat("Actual pattern:  ", df$channel[first_test:(first_test+pattern_length-1)], "\n")
if (all(df$channel[first_test:(first_test+pattern_length-1)] == test_pattern)) {
  cat("? VERIFIED: Pattern matches correctly\n")
} else {
  cat("? FAULT DETECTED: Pattern mismatch!\n")
}

pdf("d:/pdf/bit_test.pdf")
# Visualization
p <- ggplot(df, aes(x = time_step)) +
  # Channel output as steps
  geom_step(aes(y = channel, color = "Channel Output"), size = 1.2) +
  # Test active indicator (scaled for visibility)
  geom_step(aes(y = in_test * 0.8, color = "in_test Flag"), size = 1, linetype = "dashed") +
  # Highlight test regions
  geom_rect(data = df[df$in_test,],
            aes(xmin = time_step - 0.5, xmax = time_step + 0.5,
                ymin = -0.1, ymax = 1.1),
            fill = "yellow", alpha = 0.2, inherit.aes = FALSE) +
  scale_color_manual(values = c("Channel Output" = "blue", "in_test Flag" = "red")) +
  scale_y_continuous(breaks = c(0, 0.8, 1),
                     labels = c("OFF", "Test Active", "ON")) +
  labs(title = "F-DQ Channel with Periodic Bit-Pattern Test",
       subtitle = paste("Test pattern:", paste(test_pattern, collapse="?"),
                        "| Period: every", test_period, "steps"),
       x = "Time Step",
       y = "State",
       color = "Signal") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
print(p)

# Save plot (optional)
# ggsave("fdq_channel_simulation.png", p, width = 10, height = 6, dpi = 150)

# Simple fault detection function
detect_fault <- function(channel_signal, test_pattern, test_period, n_steps) {
  faults <- c()
  for (t in seq(1, n_steps - pattern_length + 1, by = test_period)) {
    actual <- channel_signal[t:(t + pattern_length - 1)]
    if (!all(actual == test_pattern)) {
      faults <- c(faults, t)
    }
  }
  return(faults)
}

# Run fault detection
faulty_cycles <- detect_fault(channel, test_pattern, test_period, n_steps)
if (length(faulty_cycles) > 0) {
  cat("\n??  FAULT DETECTED in cycles starting at steps:", faulty_cycles - 1, "\n")
} else {
  cat("\n? NO FAULTS DETECTED - All test cycles passed\n")
}
dev.off()
endsubmit;
import r=df data=workx.bit_test;
run;

proc print data=workx.bit_test;
run;

/**************************************************************************************************************************/
/* Altair SLC                                                                                                             */
/*                                                                                                                        */
/* = == == == == == == == == == == == == == ==                                                                            */
/* F-DQ CHANNEL SIMULATION RESULTS"                                                                                       */
/* = == == == == == == == == == == == == == ==                                                                            */
/*    time_step channel in_test                                                                                           */
/* 1          0       1    TRUE                                                                                           */
/* 2          1       0    TRUE                                                                                           */
/* 3          2       1    TRUE                                                                                           */
/* 4          3       0    TRUE                                                                                           */
/* 5          4       0   FALSE                                                                                           */
/* 6          5       0   FALSE                                                                                           */
/* 7          6       0   FALSE                                                                                           */
/* 8          7       0   FALSE                                                                                           */
/* 9          8       0   FALSE                                                                                           */
/* 10         9       0   FALSE                                                                                           */
/*                                                                                                                        */
/* 11        10       1    TRUE                                                                                           */
/* 12        11       0    TRUE                                                                                           */
/* 13        12       1    TRUE                                                                                           */
/* 14        13       0    TRUE                                                                                           */
/* 15        14       0   FALSE                                                                                           */
/* 16        15       0   FALSE                                                                                           */
/* 17        16       0   FALSE                                                                                           */
/* 18        17       0   FALSE                                                                                           */
/* 19        18       0   FALSE                                                                                           */
/* 20        19       0   FALSE                                                                                           */
/*                                                                                                                        */
/* 21        20       1    TRUE                                                                                           */
/* 22        21       0    TRUE                                                                                           */
/* 23        22       1    TRUE                                                                                           */
/* 24        23       0    TRUE                                                                                           */
/* 25        24       0   FALSE                                                                                           */
/* 26        25       0   FALSE                                                                                           */
/* 27        26       0   FALSE                                                                                           */
/* 28        27       0   FALSE                                                                                           */
/* 29        28       0   FALSE                                                                                           */
/* 30        29       0   FALSE                                                                                           */
/*                                                                                                                        */
/*  = == == == == == == == == == == == == == =                                                                            */
/* SUMMARY STATISTICS                                                                                                     */
/* = == == == == == == == == == == == == == ==                                                                            */
/* Total steps: 30                                                                                                        */
/* Test pattern: 1 0 1 0                                                                                                  */
/* Test period: every 10 steps                                                                                            */
/* Pattern length: 4 steps                                                                                                */
/* Total test cycles: 3                                                                                                   */
/* Steps in test mode: 12                                                                                                 */
/* Steps in normal mode: 18                                                                                               */
/* Test cycles start at time steps: 0 10 20                                                                               */
/*                                                                                                                        */
/*  = == == == == == == == == == == == == == =                                                                            */
/* VERIFY FIRST TEST CYCLE                                                                                                */
/* = == == == == == == == == == == == == == ==                                                                            */
/* Expected pattern: 1 0 1 0                                                                                              */
/* Actual pattern:   1 0 1 0                                                                                              */
/* ? VERIFIED: Pattern matches correctly                                                                                  */
/* ? NO FAULTS DETECTED - All test cycles passed                                                                          */
/**************************************************************************************************************************/

/*
| | ___   __ _
| |/ _ \ / _` |
| | (_) | (_| |
|_|\___/ \__, |
         |___/
*/

1                                          Altair SLC       16:49 Wednesday, April 29, 2026

NOTE: Copyright 2002-2025 World Programming, an Altair Company
NOTE: Altair SLC 2026 (05.26.01.00.000758)
      Licensed to Roger DeAngelis
NOTE: This session is executing on the X64_WIN11PRO platform and is running in 64 bit mode

NOTE: AUTOEXEC processing beginning; file is C:\wpsoto\autoexec.sas
NOTE: AUTOEXEC source line
1       +  ï»¿ods _all_ close;
           ^
ERROR: Expected a statement keyword : found "?"

NOTE: AUTOEXEC processing completed

1         options validvarname=v7;
2         options set=RHOME "C:\Progra~1\R\R-4.5.2\bin\r";
3         proc r;
4         submit;
5         # Simple F-DQ Channel Simulation in R
6         # Simulates one channel with periodic bit-pattern test
7
8         # Load required libraries
9         library(ggplot2)
10        library(dplyr)
11
12        # Configuration
13        test_pattern <- c(1, 0, 1, 0)  # Bit-pattern test sequence (1=ON, 0=OFF)
14        test_period <- 10                # Run test every 10 time steps
15        pattern_length <- length(test_pattern)
16        n_steps <- 30                    # Total simulation steps
17
18        # Initialize vectors
19        channel <- numeric(n_steps)      # Channel output
20        in_test <- logical(n_steps)      # Test active flag
21        process_value <- numeric(n_steps) # Process demand (normally 0)
22
23        # Simulation
24        for (t in 1:n_steps) {
25          # Check if we're at the start of a test period
26          if ((t - 1) %% test_period == 0 && (t + pattern_length - 1) <= n_steps) {
27            # Run bit-pattern test
28            for (k in 1:pattern_length) {
29              channel[t + k - 1] <- test_pattern[k]
30              in_test[t + k - 1] <- TRUE
31            }
32            # Skip normal assignment for test duration
33            next
34          }
35
36          # Normal operation (not in test)
37          if (!in_test[t]) {
38            channel[t] <- process_value[t]  # Follow process demand (0 = OFF)
39          }
40        }
41
42        # Create dataframe for analysis
43        df <- data.frame(
44          time_step = 0:(n_steps-1),
45          channel = channel,
46          in_test = in_test
47        )
48
49        # Print results
50        print("=" %>% paste(rep("=", 60), collapse=""))
51        print("F-DQ CHANNEL SIMULATION RESULTS")
52        print("=" %>% paste(rep("=", 60), collapse=""))
53        print(df)
54
55        # Summary statistics
56        cat("\n", "=" %>% paste(rep("=", 60), collapse=""), "\n")
57        cat("SUMMARY STATISTICS\n")
58        cat("=" %>% paste(rep("=", 60), collapse=""), "\n")
59        cat("Total steps:", n_steps, "\n")
60        cat("Test pattern:", test_pattern, "\n")
61        cat("Test period: every", test_period, "steps\n")
62        cat("Pattern length:", pattern_length, "steps\n")
63        cat("Total test cycles:", sum(df$in_test) / pattern_length, "\n")
64        cat("Steps in test mode:", sum(df$in_test), "\n")
65        cat("Steps in normal mode:", sum(!df$in_test), "\n")
66
67        # Find test cycles
68        test_starts <- which(df$in_test & !lag(df$in_test, default=FALSE))
69        cat("\nTest cycles start at time steps:", test_starts - 1, "\n")
70
71        # Verify pattern for first test cycle
72        cat("\n", "=" %>% paste(rep("=", 60), collapse=""), "\n")
73        cat("VERIFY FIRST TEST CYCLE\n")
74        cat("=" %>% paste(rep("=", 60), collapse=""), "\n")
75        first_test <- test_starts[1]
76        cat("Expected pattern:", test_pattern, "\n")
77        cat("Actual pattern:  ", df$channel[first_test:(first_test+pattern_length-1)], "\n")
78        if (all(df$channel[first_test:(first_test+pattern_length-1)] == test_pattern)) {
79          cat("? VERIFIED: Pattern matches correctly\n")
80        } else {
81          cat("? FAULT DETECTED: Pattern mismatch!\n")
82        }
83
84        pdf("d:/pdf/bit_test.pdf")
85        # Visualization
86        p <- ggplot(df, aes(x = time_step)) +
87          # Channel output as steps
88          geom_step(aes(y = channel, color = "Channel Output"), size = 1.2) +
89          # Test active indicator (scaled for visibility)
90          geom_step(aes(y = in_test * 0.8, color = "in_test Flag"), size = 1, linetype = "dashed") +
91          # Highlight test regions
92          geom_rect(data = df[df$in_test,],
93                    aes(xmin = time_step - 0.5, xmax = time_step + 0.5,
94                        ymin = -0.1, ymax = 1.1),
95                    fill = "yellow", alpha = 0.2, inherit.aes = FALSE) +
96          scale_color_manual(values = c("Channel Output" = "blue", "in_test Flag" = "red")) +
97          scale_y_continuous(breaks = c(0, 0.8, 1),
98                             labels = c("OFF", "Test Active", "ON")) +
99          labs(title = "F-DQ Channel with Periodic Bit-Pattern Test",
100              subtitle = paste("Test pattern:", paste(test_pattern, collapse="?"),
101                               "| Period: every", test_period, "steps"),
102              x = "Time Step",
103              y = "State",
104              color = "Signal") +
105         theme_minimal() +
106         theme(legend.position = "bottom")
107
108       # Display plot
109       print(p)
110
111       # Save plot (optional)
112       # ggsave("fdq_channel_simulation.png", p, width = 10, height = 6, dpi = 150)
113
114       # Simple fault detection function
115       detect_fault <- function(channel_signal, test_pattern, test_period, n_steps) {
116         faults <- c()
117         for (t in seq(1, n_steps - pattern_length + 1, by = test_period)) {
118           actual <- channel_signal[t:(t + pattern_length - 1)]
119           if (!all(actual == test_pattern)) {
120             faults <- c(faults, t)
121           }
122         }
123         return(faults)
124       }
125
126       # Run fault detection
127       faulty_cycles <- detect_fault(channel, test_pattern, test_period, n_steps)
128       if (length(faulty_cycles) > 0) {
129         cat("\n??  FAULT DETECTED in cycles starting at steps:", faulty_cycles - 1, "\n")
130       } else {
131         cat("\n? NO FAULTS DETECTED - All test cycles passed\n")
132       }
133       dev.off()
134       endsubmit;
NOTE: Using R version 4.5.2 (2025-10-31 ucrt) from C:\Program Files\R\R-4.5.2

NOTE: Submitting statements to R:

> # Simple F-DQ Channel Simulation in R
> # Simulates one channel with periodic bit-pattern test
>
> # Load required libraries
> library(ggplot2)
Warning message:
package 'ggplot2' was built under R version 4.5.3
> library(dplyr)
Attaching package: 'dplyr'
The following objects are masked from 'package:stats':
    filter, lag
The following objects are masked from 'package:base':
    intersect, setdiff, setequal, union
Warning message:
package 'dplyr' was built under R version 4.5.3
>
> # Configuration
> test_pattern <- c(1, 0, 1, 0)  # Bit-pattern test sequence (1=ON, 0=OFF)
> test_period <- 10                # Run test every 10 time steps
> pattern_length <- length(test_pattern)
> n_steps <- 30                    # Total simulation steps
>
> # Initialize vectors
> channel <- numeric(n_steps)      # Channel output
> in_test <- logical(n_steps)      # Test active flag
> process_value <- numeric(n_steps) # Process demand (normally 0)
>
> # Simulation
> for (t in 1:n_steps) {
+   # Check if we're at the start of a test period
+   if ((t - 1) %% test_period == 0 && (t + pattern_length - 1) <= n_steps) {
+     # Run bit-pattern test
+     for (k in 1:pattern_length) {
+       channel[t + k - 1] <- test_pattern[k]
+       in_test[t + k - 1] <- TRUE
+     }
+     # Skip normal assignment for test duration
+     next
+   }
+
+   # Normal operation (not in test)
+   if (!in_test[t]) {
+     channel[t] <- process_value[t]  # Follow process demand (0 = OFF)
+   }
+ }
>
> # Create dataframe for analysis
> df <- data.frame(
+   time_step = 0:(n_steps-1),
+   channel = channel,
+   in_test = in_test
+ )
>
> # Print results
> print("=" %>% paste(rep("=", 60), collapse=""))
> print("F-DQ CHANNEL SIMULATION RESULTS")
> print("=" %>% paste(rep("=", 60), collapse=""))
> print(df)
>
> # Summary statistics
> cat("\n", "=" %>% paste(rep("=", 60), collapse=""), "\n")
> cat("SUMMARY STATISTICS\n")
> cat("=" %>% paste(rep("=", 60), collapse=""), "\n")
> cat("Total steps:", n_steps, "\n")
> cat("Test pattern:", test_pattern, "\n")
> cat("Test period: every", test_period, "steps\n")
> cat("Pattern length:", pattern_length, "steps\n")
> cat("Total test cycles:", sum(df$in_test) / pattern_length, "\n")
> cat("Steps in test mode:", sum(df$in_test), "\n")
> cat("Steps in normal mode:", sum(!df$in_test), "\n")
>
> # Find test cycles
> test_starts <- which(df$in_test & !lag(df$in_test, default=FALSE))
> cat("\nTest cycles start at time steps:", test_starts - 1, "\n")
>
> # Verify pattern for first test cycle
> cat("\n", "=" %>% paste(rep("=", 60), collapse=""), "\n")
> cat("VERIFY FIRST TEST CYCLE\n")
> cat("=" %>% paste(rep("=", 60), collapse=""), "\n")
> first_test <- test_starts[1]
> cat("Expected pattern:", test_pattern, "\n")
> cat("Actual pattern:  ", df$channel[first_test:(first_test+pattern_length-1)], "\n")
> if (all(df$channel[first_test:(first_test+pattern_length-1)] == test_pattern)) {
+   cat("? VERIFIED: Pattern matches correctly\n")
+ } else {
+   cat("? FAULT DETECTED: Pattern mismatch!\n")
+ }
>
> pdf("d:/pdf/bit_test.pdf")
> # Visualization
> p <- ggplot(df, aes(x = time_step)) +
+   # Channel output as steps
+   geom_step(aes(y = channel, color = "Channel Output"), size = 1.2) +
+   # Test active indicator (scaled for visibility)
+   geom_step(aes(y = in_test * 0.8, color = "in_test Flag"), size = 1, linetype = "dashed") +
+   # Highlight test regions
+   geom_rect(data = df[df$in_test,],
+             aes(xmin = time_step - 0.5, xmax = time_step + 0.5,
+                 ymin = -0.1, ymax = 1.1),
+             fill = "yellow", alpha = 0.2, inherit.aes = FALSE) +
+   scale_color_manual(values = c("Channel Output" = "blue", "in_test Flag" = "red")) +
+   scale_y_continuous(breaks = c(0, 0.8, 1),
+                      labels = c("OFF", "Test Active", "ON")) +
+   labs(title = "F-DQ Channel with Periodic Bit-Pattern Test",
+        subtitle = paste("Test pattern:", paste(test_pattern, collapse="?"),
+                         "| Period: every", test_period, "steps"),
+        x = "Time Step",
+        y = "State",
+        color = "Signal") +
+   theme_minimal() +
+   theme(legend.position = "bottom")
Warning message:
Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
i Please use `linewidth` instead.
>
> # Display plot
> print(p)
>
> # Save plot (optional)
> # ggsave("fdq_channel_simulation.png", p, width = 10, height = 6, dpi = 150)
>
> # Simple fault detection function
> detect_fault <- function(channel_signal, test_pattern, test_period, n_steps) {
+   faults <- c()
+   for (t in seq(1, n_steps - pattern_length + 1, by = test_period)) {
+     actual <- channel_signal[t:(t + pattern_length - 1)]
+     if (!all(actual == test_pattern)) {
+       faults <- c(faults, t)
+     }
+   }
+   return(faults)
+ }
>
> # Run fault detection
> faulty_cycles <- detect_fault(channel, test_pattern, test_period, n_steps)
> if (length(faulty_cycles) > 0) {
+   cat("\n??  FAULT DETECTED in cycles starting at steps:", faulty_cycles - 1, "\n")
+ } else {
+   cat("\n? NO FAULTS DETECTED - All test cycles passed\n")
+ }
> dev.off()

NOTE: Processing of R statements complete

135       import r=df data=workx.bit_test;
NOTE: Creating data set 'WORKX.bit_test' from R data frame 'df'
NOTE: Data set "WORKX.bit_test" has 30 observation(s) and 3 variable(s)

136       run;
NOTE: Procedure r step took :
      real time : 2.236
      cpu time  : 0.046


137
138       proc print data=bit_test;
                          ^
ERROR: Data set "WORK.bit_test" not found
NOTE: Procedure PRINT was not executed because of errors detected
139       run;
NOTE: Procedure print step took :
      real time : 0.000
      cpu time  : 0.000


140
ERROR: Error printed on page 1

NOTE: Submitted statements took :
      real time : 2.331
      cpu time  : 0.125

/*              _
  ___ _ __   __| |
 / _ \ `_ \ / _` |
|  __/ | | | (_| |
 \___|_| |_|\__,_|

*/
