#' Generate a Simulated Dataset of Human Behavioral States Over 24 Hours
#'
#' Simulates the hourly state of a set of individuals (either bednet users or non-users) over a 24-hour period.
#' Each individual's state is assigned probabilistically based on the hour of the day and whether they use a bednet.
#'
#' @param n_individuals Integer. Number of individuals to simulate. Default is 100.
#' @param hours Integer vector. The hours of the day to simulate, typically \code{0:23}. Default is \code{0:23}.
#' @param prob_use Numeric. Probability of being a bednet user vs non-user. Default is 0.5.
#'
#' @return A data.frame with the following columns:
#' \describe{
#'   \item{individual}{Individual ID.}
#'   \item{hour}{Hour of the day (0–23).}
#'   \item{bednet_user}{Logical, indicating whether the individual is a bednet user.}
#'   \item{state}{Integer representing the individual’s state at that hour:
#'     \code{1} = outdoors, \code{2} = indoors and awake, \code{3} = indoors asleep (non-user), \code{4} = indoors asleep under bednet (user).}
#'   \item{state_label}{Factor version of \code{state} with descriptive labels.}
#' }
#'
#' @examples
#' df <- gen_df_human(n_individuals = 50)
#' df <- gen_df_human(n_individuals = 100, hours = c(0:9,17:23))
#' head(df)
#'
#' @export
gen_df_human <- function(n_individuals = 100, hours = c(0:23), prob_use = 0.5){
	# Assign each individual as a bednet user (TRUE) or not (FALSE)
	bednet_users <- sample(c(TRUE, FALSE), n_individuals, replace = TRUE, prob = c(prob_use,1-prob_use))
	individual_info <- data.frame(individual = 1:n_individuals,
																bednet_user = bednet_users)

	# Function to assign state based on hour and bednet usage
	assign_state <- function(hour, is_user) {

		if (hour >= 6 & hour < 8) {
			probs <- c(0.3, 0.7)  # outdoors, awake indoors
			state <- sample(c(1,2), 1, prob = probs)
		} else if (hour >= 8 & hour < 18) {
			probs <- c(0.8, 0.2)
			state <- sample(c(1,2), 1, prob = probs)
		} else if (hour >= 18 & hour < 21) {
			probs <- c(0.5, 0.5)
			state <- sample(c(1,2), 1, prob = probs)
		} else if (hour >= 21 & hour < 23) {
			probs <- c(0.1, 0.3, 0.6)  # awake indoors, asleep (with or without bednet)
			state <- sample(c(1, 2, 3), 1, prob = probs)
			if (state == 3) {
				state <- if (is_user) 4 else 3
			}
		} else { #
			probs <- c(0.05, 0.05, 0.9)
			state <- sample(c(1, 2, 3), 1, prob = probs)
			if (state == 3) {
				state <- if (is_user) 4 else 3
			}
		}
		return(state)
	}

	# Create full grid and merge bednet user info
	df <- expand.grid(individual = 1:n_individuals, hour = hours)
	df <- merge(df, individual_info, by = "individual")

	# Assign state based on hour and bednet usage
	df$state <- mapply(assign_state, df$hour, df$bednet_user)

	# Add factor label
	df$state_label <- factor(df$state,
													 levels = 1:4,
													 labels = c("outdoors", "indoors_awake", "indoors_asleep", "indoors_asleep_bednet"))
	return(df)
}


#' Generate a Simulated Dataset of Hourly Mosquito Biting Rates
#'
#' Simulates the number of Anopheles mosquito bites occurring indoors and outdoors for each hour of the day.
#' Biting rates follow predefined lambda patterns that reflect typical mosquito activity (e.g., low during the day, peaking at night).
#'
#' @return A data.frame with the following columns:
#' \describe{
#'   \item{hour}{Hour of the day (0–23).}
#'   \item{indoor_bites}{Simulated number of mosquito bites occurring indoors for each hour.}
#'   \item{outdoor_bites}{Simulated number of mosquito bites occurring outdoors for each hour.}
#' }
#'
#' @examples
#' df_bites <- gen_df_mosquito()
#' head(df_bites)
#' @importFrom stats rpois
#' @export
gen_df_mosquito <- function(){
	# Define hours of the day
	hours <- 0:23

	# Example pattern: low biting during day, higher at night
	# Define indoor and outdoor lambda vectors (mean number of bites per hour)
	indoor_lambda <- c(rep(3.5, 6),    # 0-5: low
										 rep(1.0, 2),    # 6-7: early morning
										 rep(0.2, 10),   # 8-17: daytime
										 rep(1.0, 2),    # 18-19: evening
										 rep(2.0, 2),    # 20-21: rising
										 rep(3.5, 2)    # 22-23: peak biting time

	)

	outdoor_lambda <- c(rep(3.0, 6),    # 0-5: steady
											rep(0.5, 2),    # 6-7: early morning
											rep(0.1, 10),   # 8-17: daytime
											rep(2.0, 2),    # 18-19: evening
											rep(4.0, 2),    # 20-21: high
											rep(5.0, 2)     # 22-23: peak outdoors
	)

	# Simulate number of bites for each hour and location
	indoor_bites <- rpois(length(hours), lambda = indoor_lambda)
	outdoor_bites <- rpois(length(hours), lambda = outdoor_lambda)

	# Combine into a dataframe
	df_bites <- data.frame(
		hour = hours,
		indoor_bites = indoor_bites,
		outdoor_bites = outdoor_bites
	)
	return(df_bites)
}


#' Calculate Hourly Mosquito Biting Exposure for Bednet Users and Non-Users
#'
#' Using a model of behavioral interactions, computes the hourly exposure to mosquito bites for both bednet users and non-users,
#' based on their location (indoors, outdoors, under a net) and mosquito biting rates.
#' For users, exposure is adjusted for the protective effect of bednets.
#'
#' @param df A dataframe with individual-level data, including columns:
#'   \itemize{
#'     \item \code{individual} – individual ID,
#'     \item \code{hour} – hour of the day (0–23),
#'     \item \code{state} – activity/location state (1 = outdoors, 2 = indoors awake,
#'     3 = indoors asleep, 4 = indoors asleep under bednet),
#'     \item \code{bednet_user} – logical indicator of bednet use (TRUE/FALSE).
#'   }
#' @param df_bites A dataframe of biting rates by hour, with columns:
#'   \itemize{
#'     \item \code{hour} – hour of the day (0–23),
#'     \item \code{indoor_bites} – number of bites indoors,
#'     \item \code{outdoor_bites} – number of bites outdoors.
#'   }
#' @param protection A numeric value (default = 0.92, according to \insertCite{corbelFieldEfficacyNew2010;textual}{ExpBites} for Permanet 2 ITNs) indicating the proportion of bites prevented by bednet use.
#'
#' @details
#' The mathematical model of behavioral interactions is an extension of the \insertCite{killeenQuantifyingBehaviouralInteractions2006;textual}{ExpBites} model as previously described in \insertCite{geissbuhlerInterdependenceDomesticMalaria2007;textual}{ExpBites} and \insertCite{moirouxHumanExposureEarly2014;textual}{ExpBites}
#' 
#' @references 
#'  \insertAllCited{}
#' 
#' @return A dataframe with one row per hour (0–23) and the following columns:
#'   \itemize{
#'     \item \code{Bi_t} – indoor biting rate,
#'     \item \code{Bo_t} – outdoor biting rate,
#'     \item \code{N} – number of human individuals,
#'     \item \code{Np} – number of human individuals that are net users,
#'     \item \code{It} – proportion of people indoors (users and non-users),
#'     \item \code{Eui} – mean exposure of non-users indoors,
#'     \item \code{Euo} – mean exposure of non-users outdoors,
#'     \item \code{Eu} – total mean exposure of non-users,
#'     \item \code{p_in} – proportion of people (users) being indoors,
#'     \item \code{p_net} – proportion of people (users) being asleep (under net),
#'     \item \code{Epi} – mean exposure of users indoors (not under net),
#'     \item \code{Epn} – mean exposure of users indoors under bednet (adjusted by protection),
#'     \item \code{Epo} – mean exposure of users outdoors,
#'     \item \code{Epp} – mean exposure prevented by bednet use,
#'     \item \code{Ep} – total mean exposure of users.
#'   }
#'
#' @examples
#' # generate fake data 
#' df <- gen_df_human(n_individuals = 100, hours = c(0:9,17:23))
#' df_bites <- gen_df_mosquito() 
#' # calculate mean hourly exposure to bites
#' exposure_results <- calculate_Exp(df, df_bites)
#'
#' @export
calculate_Exp <- function(df, df_bites, protection = 0.92) {

	# Filter for users only
	df_users <- subset(df, bednet_user == TRUE)


	# Initialize result dataframe
	result <- data.frame(hour = 0:23)

	for (t in 0:23) {
		# Subset for this hour
		df_hour <- subset(df, hour == t)

		# Number of individuals
		N <- nrow(df_hour)

		# Proportion people indoors (state 2 or 3)
		It <- mean(df_hour$state %in% c(2, 3, 4))

		# Get Bi,t and Bo,t from df_bites
		Bi_t <- df_bites$indoor_bites[df_bites$hour == t]  # biting rate indoors
		Bo_t <- df_bites$outdoor_bites[df_bites$hour == t] # biting rate outdoors

		# Compute mean exposure values (unprotected)
		Eui <- It * Bi_t         # mean exposure unprotected indoors
		Euo <- (1 - It) * Bo_t   # mean exposure unprotected outdoors
		Eu <- Eui + Euo          # mean exposure unprotected

		# Store
		result$Bi_t[result$hour == t] <- Bi_t
		result$Bo_t[result$hour == t] <- Bo_t
		result$N[result$hour == t] <- N
		result$It[result$hour == t] <- It
		result$Eui[result$hour == t] <- Eui
		result$Euo[result$hour == t] <- Euo
		result$Eu[result$hour == t] <- Eu

		# subset for this our, only users
		df_hour_u <- subset(df_users, hour == t)

		Np <- nrow(df_hour_u)
		if (Np == 0) next  # just in case

		# Count states
		p_net <- mean(df_hour_u$state == 4)           # proportion user indoors and under bed net
		p_in <- mean(df_hour_u$state %in% c(2, 3))      # proportion user indoors but not under net
		p_out <- mean(df_hour_u$state == 1)           # proportion user outdoors

		# Exposure of users calculation
		Epi <- p_in * Bi_t                     # mean exposure ITN-protected (user) indoor (not under net)
		Epn <- p_net * Bi_t * (1 - protection) # mean exposure ITN-protected (user) indoor under net
		Epo <- p_out * Bo_t                    # mean exposure ITN-protected (user) outdoor
		Epp <- p_net * Bi_t * protection       # mean exposure prevented by using net
		Ep <- Epi + Epn + Epo                  # mean exposure ITN-protected (user)

		# Store
		result$Np[result$hour == t] <- Np
		result$p_net[result$hour == t] <- p_net
		result$p_in[result$hour == t] <- p_in
		result$Epi[result$hour == t] <- Epi
		result$Epn[result$hour == t] <- Epn
		result$Epo[result$hour == t] <- Epo
		result$Epp[result$hour == t] <- Epp
		result$Ep[result$hour == t] <- Ep
	}

	return(result)
}



#' Summarize Exposure to Mosquito Bites Over 24h and a Time Interval
#'
#' Computes daily (24h) and interval-specific exposure statistics for both non-users and bednet users,
#' based on the output of \code{\link{calculate_Exp}}. Protective efficacy of bednets
#' and proportion of exposure indoors or during the specified interval are computed.
#'
#' @param data A dataframe returned by \code{calculate_Exp()}, including columns:
#'   \itemize{
#'     \item \code{Eui}, \code{Euo}, \code{Eu} – indoor, outdoor, and total exposure for non-users,
#'     \item \code{Epi}, \code{Epn}, \code{Epo}, \code{Epp}, \code{Ep} – indoor, indoor under net, outdoor, prevented, and total exposure for users,
#'     \item \code{hour} – hour of the day (0–23).
#'   }
#' @param interval A numeric vector of length 2 giving the start and end hour (inclusive) of the time interval,
#'   e.g., \code{c(22, 5)} for 10pm to 5am. Wraps over midnight if needed.
#' @details
#' The mathematical model of behavioral interactions is an extension of the \insertCite{killeenQuantifyingBehaviouralInteractions2006;textual}{ExpBites} model as previously described in \insertCite{geissbuhlerInterdependenceDomesticMalaria2007;textual}{ExpBites} and \insertCite{moirouxHumanExposureEarly2014;textual}{ExpBites}
#' 
#' @references 
#'  \insertAllCited{}
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{type}{Character string indicating the category of summary: \code{"non_user_daily"}, \code{"user_daily"},
#'  							\code{"non_user_interval"}, \code{"user_interval"}, \code{"interval_vs_daily"} or \code{"net_efficacy"}.}
#'   \item{output}{Name of the exposure component or metric (e.g., \code{"Eui"}, \code{"Euo"}, \code{"Ep"},
#'  							\code{"Epp"}, \code{"prop_indoor"}, etc., \code{"prop_interval_non_user"}, \code{"prop_interval_user"} and \code{"prop_prevented"}).}
#'   \item{value}{Numeric value corresponding to the output for the given type.}
#' }
#'
#' @examples
#' # generate fake data 
#' df <- gen_df_human(n_individuals = 100, hours = c(0:9,17:23))
#' df_bites <- gen_df_mosquito() 
#' # calculate mean hourly exposure to bites
#' exposure_results <- calculate_Exp(df, df_bites)
#' # summarise exposure estimates
#' summarise_exposure(exposure_results, interval = c(22, 5))
#'
#' @export
summarise_exposure <- function(data, interval = c(22, 5)){
	# Helper to extract hours in interval, handling wrap-around
	get_interval_hours <- function(start, end) {
		if (start <= end) {
			return(start:end)
		} else {
			return(c(start:23, 0:end))
		}
	}

	interval_hours <- get_interval_hours(interval[1], interval[2])

	# Daily totals – Non-users
	non_user_daily <- with(data, {
		Eui_total <- sum(Eui, na.rm = TRUE)
		Euo_total <- sum(Euo, na.rm = TRUE)
		list(
			Eui = Eui_total,
			Euo = Euo_total,
			Eu = Eui_total + Euo_total,
			prop_indoor = Eui_total / (Eui_total + Euo_total)
		)
	})

	# Daily totals – Users
	user_daily <- with(data, {
		Epi_total <- sum(Epi, na.rm = TRUE)
		Epn_total <- sum(Epn, na.rm = TRUE)
		Epo_total <- sum(Epo, na.rm = TRUE)
		Epp_total <- sum(Epp, na.rm = TRUE)
		list(
			Epi = Epi_total,
			Epn = Epn_total,
			Epo = Epo_total,
			Epp = Epp_total,
			Ep = Epi_total + Epo_total + Epn_total,
			prop_indoor = (Epi_total + Epn_total) / (Epi_total + Epo_total + Epn_total)
		)
	})

	# Interval data
	data_interval <- data[data$hour %in% interval_hours, ]

	# Interval totals – Non-users
	non_user_interval <- with(data_interval, {
		Eui_total <- sum(Eui, na.rm = TRUE)
		Euo_total <- sum(Euo, na.rm = TRUE)
		list(
			Eui = Eui_total,
			Euo = Euo_total,
			Eu = Eui_total + Euo_total,
			prop_indoor = Eui_total / (Eui_total + Euo_total)
		)
	})

	# Interval totals – Users
	user_interval <- with(data_interval, {
		Epi_total <- sum(Epi, na.rm = TRUE)
		Epn_total <- sum(Epn, na.rm = TRUE)
		Epo_total <- sum(Epo, na.rm = TRUE)
		Epp_total <- sum(Epp, na.rm = TRUE)
		list(
			Epi = Epi_total,
			Epn = Epn_total,
			Epo = Epo_total,
			Epp = Epp_total,
			Ep = Epi_total + Epo_total + Epn_total,
			prop_indoor = (Epi_total+Epn_total) / (Epi_total + Epo_total + Epn_total)
		)
	})

	result <- bind_rows(
		as_tibble(non_user_daily) %>% mutate(type = "non_user_daily") %>% pivot_longer(!type, names_to = "output", values_to = "value"),
		as_tibble(user_daily) %>% mutate(type = "user_daily") %>% pivot_longer(!type, names_to = "output", values_to = "value"),
		as_tibble(non_user_interval) %>% mutate(type = "non_user_interval") %>% pivot_longer(!type, names_to = "output", values_to = "value"),
		as_tibble(user_interval) %>% mutate(type = "user_interval") %>% pivot_longer(!type, names_to = "output", values_to = "value")
	) %>%
		relocate(type) %>%
		add_row(type = "interval_vs_daily", output = "prop_interval_non_user",
						value = .$value[.$output=="Eu" & .$type=="non_user_interval"] / .$value[.$output=="Eu" & .$type=="non_user_daily"]) %>%
		add_row(type = "interval_vs_daily", output = "prop_interval_user",
						value = .$value[.$output=="Ep" & .$type=="user_interval"] / .$value[.$output=="Ep" & .$type=="user_daily"]) %>%
		add_row(type = "net_efficacy", output = "prop_prevented",
						value = .$value[.$output=="Epp" & .$type=="user_daily"] /
							(value = .$value[.$output=="Ep" & .$type=="user_daily"] + .$value[.$output=="Epp" & .$type=="user_daily"]))

	return(result)
}



#' Plot Exposure to Mosquito Bites by Hour for Bednet Users
#'
#' Creates a stacked area chart showing the hourly distribution of exposure to mosquito bites
#' among bednet users, broken down by exposure type: outdoors, indoors (no net), indoors (under net),
#' and prevented exposure due to bednet use. The hours are centered on midnight for better visualization.
#'
#' @param data A dataframe resulting from the \code{\link{calculate_Exp}} function, containing columns:
#'   \itemize{
#'     \item \code{hour} – hour of the day (0–23),
#'     \item \code{Epo} – exposure outdoors,
#'     \item \code{Epi} – exposure indoors (not under a net),
#'     \item \code{Epn} – exposure indoors under a net,
#'     \item \code{Epp} – exposure prevented by net use.
#'   }
#' @param cPalette A character vector of color values (hex codes or color names) used to fill each exposure category.
#'   Must be of length 4 and will be applied in the following order:
#'   \code{Prevented}, \code{Indoor (under net)}, \code{Indoor (no net)}, \code{Outdoor}.
#'   Defaults to \code{c("#000000", "#E69F00", "#56B4E9", "#009E73")}.
#'
#' @return A \code{ggplot2} stacked area chart object showing the distribution of exposure types across centered hours.
#'
#' @examples
#' # generate fake data 
#' df <- gen_df_human(n_individuals = 100, hours = c(0:9,17:23))
#' df_bites <- gen_df_mosquito() 
#' # calculate mean hourly exposure to bites
#' exposure_results <- calculate_Exp(df, df_bites)
#' # plot
#' plot_exposure(exposure_results)
#'
#' @import ggplot2 dplyr tidyr
#' @export
plot_exposure <- function(data, cPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73")){
	# Assume result is the output of calculate_Exp()
	# Center hour on midnight (shift so 0h is in the middle: e.g., move 12–23 before 0–11)
	plot_data <- data %>%
		mutate(hour_centered = (hour + 12) %% 24) %>%
		select(hour_centered, Epo, Epi, Epn, Epp) %>%
		pivot_longer(cols = c(Epo, Epi, Epn, Epp),
								 names_to = "component",
								 values_to = "exposure")

	# Make component labels more readable
	plot_data$component <- factor(plot_data$component,
																levels = c("Epp",  "Epn", "Epi", "Epo"),
																labels = c("Prevented",  "Indoor (under net)","Indoor (no net)", "Outdoor"))

	# Plot
	plot <- ggplot(plot_data, aes(x = hour_centered, y = exposure, fill = component)) +
		geom_area(alpha = 0.4, color = "black", size = 0.2) +
		scale_x_continuous(breaks =  c(5,10,15,20), labels=c("17","22","3","8"), name = "Hour") +
		ylab("Exposure to mosquitoes (bites/person/hour)") +
		scale_fill_manual(values=cPalette, name = "Exposure :")

	return(plot)
}


#' Plot Human and Mosquito Behaviors by Hour
#'
#' Generates a dual-axis plot showing the hourly variation in human behavior (proportion of individuals indoors and under bednets)
#' and mosquito biting behavior (biting rates indoors and outdoors). Human behavior is shown as a stacked area plot on the
#' primary Y-axis, while mosquito biting rates are shown as lines on the secondary Y-axis. Hours are centered on midnight.
#'
#' @param data A dataframe (typically the output of \code{\link{calculate_Exp}}) containing the following columns:
#'   \itemize{
#'     \item \code{hour} – hour of the day (0–23),
#'     \item \code{p_in} – proportion of users indoors (not under a net),
#'     \item \code{p_net} – proportion of users under a net,
#'     \item \code{Bi_t} – indoor mosquito biting rate,
#'     \item \code{Bo_t} – outdoor mosquito biting rate.
#'   }
#'
#' @return A \code{ggplot2} plot object with a stacked area chart for human behavior (on the primary Y-axis)
#'   and line plots for mosquito biting rates (on the secondary Y-axis), with hours centered on midnight.
#'
#' @examples
#' # generate fake data 
#' df <- gen_df_human(n_individuals = 100, hours = c(0:9,17:23))
#' df_bites <- gen_df_mosquito() 
#' # calculate mean hourly exposure to bites
#' exposure_results <- calculate_Exp(df, df_bites)
#' # plot
#' plot_behaviors(exposure_results)
#'
#' @import ggplot2 dplyr tidyr
#' @export
plot_behaviors <- function(data){
	plot_people <- data %>%
		mutate(hour_centered = (hour + 12) %% 24) %>%
		select(hour_centered, p_in, p_net) %>%
		pivot_longer(cols = c(p_in, p_net),
								 names_to = "people",
								 values_to = "value") %>%
		mutate(people = factor(people, levels=c("p_in", "p_net"), labels=c("Indoors", "Under nets") )) %>%
		mutate(value = value*100)

	plot_mosq <- data %>%
		mutate(hour_centered = (hour + 12) %% 24) %>%
		select(hour_centered, Bi_t, Bo_t) %>%
		pivot_longer(cols = c(Bi_t, Bo_t),
								 names_to = "mosquitoes",
								 values_to = "value") %>%
		mutate(mosquitoes = factor(mosquitoes, levels=c("Bi_t", "Bo_t"), labels=c("Indoors", "Outdoors") ))

	# scale factor used to rescale values and y-axis
	scale_factor <- max(plot_mosq$value, na.rm = TRUE)/100

	plot <- ggplot2::ggplot() +
		ggplot2::geom_area(data = plot_people, aes(x=hour_centered, y=value, fill=people),colour="grey", size=.3, alpha=.4) +
		ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~.*scale_factor, name = "hourly biting rate (bites/person/hour)"), name = "% of people indoor or under net") +
		ggplot2::geom_line(data = plot_mosq, ggplot2::aes(x=hour_centered, y = value/scale_factor, linetype = mosquitoes), size=0.6) +
		ggplot2::scale_linetype_manual(values = c('Indoors' = 1,'Outdoors' = 4)) +
		ggplot2::scale_x_continuous(breaks =  c(5,10,15,20),labels=c("17","22","3","8"), name = "Hour")

	return(plot)
}

