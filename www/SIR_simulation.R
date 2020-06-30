## CONSTANTS

# Population size 
N <- 1e6 

# Rate at which person stays in the infectious compartment (disease specific and tracing specific)
gamma <- 1/5 

# Initial number of infected people
n_init <- 10


# Function to compute the derivative of the ODE system
# -----------------------------------------------------------
#  t - time
#  y - current state vector of the ODE at time t
#  parms - Parameter vector used by the ODE system (beta, gamma)

sir <- function(t, y, parms, 
                social_dist_period, 
                reduction) {
    
    beta0 <- parms[1]
    gamma <- parms[2]
    
    # Reduce contact rate 
    beta_t <- if_else(t <= social_dist_period[1], 
                      beta0,
                      if_else(t <= social_dist_period[2], 
                              beta0 * reduction[1],
                              beta0 * reduction[2]
                      )
    )
    
    S <- y[1]
    I <- y[2]
    
    return(list(c(S = -beta_t * S * I, 
                  I =  beta_t * S * I - gamma * I)))
}


## we assume that some globals exist...
solve_ode <- function(sdp, red, typ, beta, max_time) {
    
    # Grid where to evaluate
    times <- reactive({ seq(0, max_time, by = 0.1) })
    
    ode_solution <- lsoda(y = c(N - n_init, n_init), 
                          times = times(), 
                          func  = sir, 
                          parms = c(beta, gamma), 
                          social_dist_period = sdp,
                          reduction = red) %>%
        as.data.frame() %>%
        setNames(c("t", "S", "I")) %>%
        mutate(beta = beta, 
               gama = gamma,
               R0 = N * beta / gamma, 
               s  = S / N, 
               i  = I / N, 
               type = typ)
    
    daily <- ode_solution %>%
        filter(t %in% seq(0, max_time, by = 1)) %>%
        mutate(C = if_else(row_number() == 1, 0, lag(S, k = 1) - S), 
               c = C / N)
    
    daily
}





# add results with intervention and plot
run <- function(sdp, red, r0, max_time) {
    
    beta <- r0 / N * gamma
    
    ode_solution_daily <- solve_ode(
        sdp = c(0, max_time),  # social_dist_period
        red = c(1, 1),         # reduction
        typ = "without", 
        beta = beta, 
        max_time
    )
    
    # solve with interventions
    ode_solution2_daily <- solve_ode(
        sdp = sdp,
        red = red,
        typ = "with", 
        beta = beta, 
        max_time
    )
    
    # Combine the two solutions into one dataset
    ode_df <- rbind(ode_solution_daily, ode_solution2_daily)
    
}


plot_result <- function(ode_df, sdp, max_time, y_axis_fixed) {    
    
    # The final size in the two cases:
    final_sizes <- ode_df %>%
        group_by(type) %>%
        filter(row_number() == n()) %>%
        mutate("final fraction" = scales::percent(1 - s, accuracy = 1)) %>%
        select("final fraction", interventions = type) %>% 
        arrange(desc(interventions))
    
    # Plot
    if (y_axis_fixed) {
        y_max <- 0.09
    } else {
        y_max <- max(ode_df$c, na.rm = TRUE) * 1.05
    }
    y_arrow <- y_max * 0.975
    y_text  <- y_arrow + y_max * 0.01 
    col_sdp <- "lightblue3"
    
    x_labs <- sort(c(0, 100, 200, 300, 365, sdp))
    
    
    pp <- ggplot(ode_df, 
                 aes(x = t, 
                     y = 0, 
                     xend = t, 
                     yend = c, 
                     color = type)) + 
        geom_segment(alpha = 0.7) + 
        geom_line(aes(x = t, y = c)) + 
        labs(
            x = "Days", 
            y = NULL, 
            subtitle = "Daily new cases in % of the population", 
            caption  = "sdp: social distance period") +
        scale_x_continuous(labels = x_labs, 
                           breaks = x_labs) +
        scale_y_continuous(labels = scales::percent,
                           limits = c(0, y_max)) +
        theme(axis.text.y = element_blank(),
              axis.ticks  = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_color_brewer(name = "Interventions", 
                           type = "qual", 
                           palette = 1, 
                           guide = guide_legend(reverse = TRUE)) +
        # sdp 
        geom_vline(xintercept = sdp, lty = 2, color = "darkgray") +
        geom_text(aes(x = sdp[1] + (sdp[2] - sdp[1])/2, 
                      y = y_text, 
                      label = "sdp 1"),
                  vjust = 0,
                  color = col_sdp) +   
        geom_text(aes(x = sdp[2] + 40, 
                      y = y_text, 
                      label = "sdp 2"),
                  vjust = 0,
                  color = col_sdp) +   
        geom_segment(aes(
            x = sdp[1],
            y = y_arrow, 
            xend = sdp[2] * 0.99, # shorten
            yend = y_arrow
        ),
        size = 0.3, 
        color = col_sdp,
        arrow = arrow(length = unit(2, "mm"))) +
        geom_segment(aes(
            x = sdp[2]*1.01, # shorten
            y = y_arrow, 
            xend = max_time,
            yend = y_arrow
        ),
        size = 0.3, 
        color = col_sdp,
        arrow = arrow(length = unit(2, "mm")))  +
        # Add final size as table
        annotation_custom(tableGrob(final_sizes, 
                                    rows = NULL,
                                    theme = ttheme_minimal(
                                        core    = list(fg_params = list(hjust = 0, x = 0.1)),
                                        rowhead = list(fg_params = list(hjust = 0, x = 0))
                                    )),
                          xmin = max_time * 0.6,
                          xmax = max_time,
                          ymin = y_max * 0.7,
                          ymax = y_max * 0.9
        )
    
    print(pp)
}
