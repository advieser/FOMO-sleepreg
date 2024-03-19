# --- Defining Processes ---

# Process C
# t: clock time in decimal hours since start of experiment
# Fix for all agents; min and max as specified in paper
process_c <- function(t) {
  -0.5 * (sin((t + 2) * pi/12) - 1)
}

# Process S
# t: clock time in decimal hours since start of experiment
# state: current state at t to asses which function to choose
# last_sleep_onset: while asleep, for correct slope at t
# last_awakening: while awake, for correct slope at t
process_s_asleep <- function(t, last_sleep_onset) {
  1.36 * 1.5 ^ (-t + last_sleep_onset)
}
process_s_awake <- function(t, last_awakening) {
  0.33 * sqrt(t - last_awakening)
}
process_s <- function(t, state, last_awakening, last_sleep_onset) {
  if (state == 1) {
    process_s_awake(t, last_awakening)
  } else if (state == 0) {
    process_s_asleep(t, last_sleep_onset)
  }
}

# Sleep Propensity
# Sum of S and C
sleep_propensity <- function(t, state, last_awakening, last_sleep_onset) {
  process_s(t, state, last_awakening, last_sleep_onset) + process_c(t)
}


# --- Impelement Model ---

# An agent with start parameters:
# - state (asleep = 0 / awake = 1)
# - sleep_onset (last sleep onset, for first sleep propensity)
# - awakening (last awakening, for first sleep propensity)
# - bedtime: fixed time to go to bed
agent_start <- list(start_state = 1, # is awake in beginning
                    start_sleep_onset = NA,
                    start_awakening = c(6),
                    bedtime = 23)

# set up time steps in hours, starting with beginning of experiment
time_steps <- seq(7, 72, by = 1) # starting with 7:00 on travel day

# for each time point:
# - calculate current sleep propensity
# - if awake: check wehther agent should go to sleep
# - if asleep: check whether agent should wake up
# - save time points than agent changes state
# Return: data.frame with all parameters for all time steps
simulate_trial <- function(agent, time_steps) {
  # pre-allocate output
  trial <- data.frame(
    time_steps = time_steps,
    states = c(agent[["start_state"]], numeric(length(time_steps) - 1)),
    last_awakening = c(agent[["start_awakening"]], numeric(length(time_steps) - 1)),
    last_sleep_onset = c(agent[["start_sleep_onset"]], numeric(length(time_steps) - 1)),
    sleep_propensity = numeric(length(time_steps))
  )

  for (i in seq_along(time_steps)) {
    # calculate sleep propensity
    trial[[i, "sleep_propensity"]] <- sleep_propensity(
      t = time_steps[[i]],
      state = trial[[i, "states"]],
      last_awakening = trial[[i, "last_awakening"]],
      last_sleep_onset = trial[[i, "last_sleep_onset"]])

    # wake-up check, if asleep
    if (trial[[i, "states"]] == 0) {  # currently asleep
      # should actually wake up then exactly 0, however bc of discrete time steps
      # check for smaller or equal
      if (trial[[i, "sleep_propensity"]] <= 0) {
        trial[[i + 1, "states"]] <- 1  # wake up!
        trial[[i + 1, "last_awakening"]] <- time_steps[[i]] # note last awaenking
      } else {
        trial[[i + 1, "states"]] <- 0
        trial[[i + 1, "last_awakening"]] <- trial[[i, "last_awakening"]]
      }
      trial[[i + 1, "last_sleep_onset"]] <- trial[[i, "last_sleep_onset"]]  # still last one
    }

    # sleep onset check, if awake
    if (trial[[i, "states"]] == 1) {
      if (time_steps[[i]] %% 24 >= agent[["bedtime"]]) {
        trial[[i + 1, "states"]] <- 0  # zZz, falling asleep
        trial[[i + 1, "last_sleep_onset"]] <- time_steps[[i]] # note last sleep onset
      } else {
        trial[[i + 1, "states"]] <- 1  # stay up a little bit longer
        trial[[i + 1, "last_sleep_onset"]] <- trial[[i, "last_sleep_onset"]]  # still last one
      }
      trial[[i + 1, "last_awakening"]] <- trial[[i, "last_awakening"]]
    }
  }
  trial
}

# --- Simulation ---
simulate_trial(agent = agent_start, time_steps = time_steps)
