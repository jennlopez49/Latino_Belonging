### histograms of linked fate across contexts

ggplot(cmps_lat_16$variables$ICI_Reverse) 

stigma_facet <- ggplot(cmps_lat_16$variables, aes(x = Linked_Fate)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ ICI_Reverse, scales = "free_y") +  # Create separate histograms for each stigma category
  labs(title = "Distribution of Linked Fate by Stigma Context",
       x = "Linked Fate",
       y = "Count") +
  theme_minimal()

ggsave(
  filename = "lf_stigma.png",  # File name with extension
  plot = stigma_facet,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

stigma_facet_int <- ggplot(cmps_lat_16$variables, aes(x = Inclusion_Internal, fill = ICI_Reverse)) +
  geom_density(alpha = 0.6) +  # Add density plot for smoother visualization
  facet_wrap(~ ICI_Reverse, scales = "fixed") +  # Ensure same y-axis across facets
  labs(title = "Density of Internal Inclusion by Stigma Context",
       x = "Internal Inclusion",
       y = "Density") +
  theme_minimal()

stigma_facet_int

ggsave(
  filename = "dens_int.png",  # File name with extension
  plot = stigma_facet_int,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

stigma_facet_int_1_hist <- ggplot(subset(cmps_lat_16$variables, ICI_Reverse == 1), aes(x = Inclusion_Internal)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Internal Inclusion for ICI_Reverse = 1",
       x = "Internal Inclusion",
       y = "Count") +
  theme_minimal()

stigma_facet_ext <- ggplot(cmps_lat_16$variables, aes(x = Inclusion_External, fill = ICI_Reverse)) +
  geom_density(alpha = 0.6) +  # Add density plot for smoother visualization
  facet_wrap(~ ICI_Reverse, scales = "fixed") +  # Ensure same y-axis across facets
  labs(title = "Density of External Inclusion by Stigma Context",
       x = "External Inclusion",
       y = "Density") +
  theme_minimal()

stigma_facet_ext

ggsave(
  filename = "dens_ext.png",  # File name with extension
  plot = stigma_facet_ext,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

persdic.fig <- ggplot(cmps_lat_16$variables, aes(x = factor(conc_lat_index_14_16), 
                                                 fill = factor(Discrimination_Scale))) +
  geom_bar() +  # This will show the proportion of 0/1 across context levels
  scale_y_continuous(labels = scales::percent) +  # Converts y-axis to percentages
  labs(title = "Personal Discrimination by Policy Context",
       x = "Anti-Immigrant Policy Context (Reverse ICI)",
       y = "Proportion of Reported Personal Discrimination") +
  theme_minimal()

groupdisc.fig <- ggplot(
  cmps_lat_16$variables,
  aes(
    x = factor(conc_lat_index_14_16),
    fill = factor(Latino_Disc)
  )
) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(
      # compute proportion *within each x level (context)*
      label = scales::percent(after_stat(count / tapply(count, x, sum)[x]), accuracy = 1)
    ),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_fill_manual(
    name = "Discrimination against Latinos",
    values = c("1" = "lightblue", "2" = "lightgreen", "3" = "orange", "4" = "purple"),
    labels = c("None", "A little", "Some", "A lot")
  ) +
  labs(
    title = "Group Discrimination by Policy Context",
    x = "Anti-Immigrant Stigma Context",
    y = "Proportion of Perceived Discrimination against Latinos"
  ) +
  theme_minimal()

ggsave(
  filename = "disc_context.png",  # File name with extension
  plot = groupdisc.fig,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)


perspdisc.fig <- ggplot(
  cmps_lat_16$variables,
  aes(
    x = factor(conc_lat_index_14_16),
    fill = factor(Discrimination_Scale)
  )
) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(
      # compute proportion *within each x level (context)*
      label = scales::percent(after_stat(count / tapply(count, x, sum)[x]), accuracy = 1)
    ),
    position = position_fill(vjust = 0.5),
    color = "black",
    size = 3
  ) +
  scale_fill_manual(
    name = "Discrimination against Latinos",
    values = c("0" = "lightblue", "1" = "purple"),
    labels = c("No", "Yes")
  ) +
  labs(
    title = "Personal Discrimination by Policy Context",
    x = "Anti-Immigrant Stigma Context",
    y = "Reported Personal Experience with Discrimination"
  ) +
  theme_minimal()

ggsave(
  filename = "pers_disc.png",  # File name with extension
  plot = perspdisc.fig,                             # Plot object
  width = 10,                             # Width in inches
  height = 8,                             # Height in inches
  dpi = 300                               # Resolution in dots per inch
)

ggplot(cmps_lat_16$variables, aes(x = factor(Belong_US), fill = factor(More_Than_SecondGen))) +
  geom_bar() +  # This will show the proportion of 0/1 across context levels
  scale_y_continuous() +  # Converts y-axis to percentages
  labs(title = "Internal Inclusion by Generation",
       x = "Generation",
       y = "Internal Inclusion (Belonging)") + 
  # scale_fill_manual(name = "Discrimination against Latinos",
  #                   values = c("1" = "",
  #                              "2" = "lightgreen", 
  #                              "3" = "orange", 
  #                              "4" = "purple"),
  #                   labels = c("None", "A little", 
  #                              "Some", "A lot")) +
  theme_minimal()


ggplot(cmps_lat_16$variables, aes(x = factor(Valued_Respected_US), fill = factor(More_Than_SecondGen))) +
  geom_bar() +  # This will show the proportion of 0/1 across context levels
  scale_y_continuous() +  # Converts y-axis to percentages
  labs(title = "External Inclusion by Generation",
       x = "Generation",
       y = "Internal Inclusion (Valued, Respected)") + 
  # scale_fill_manual(name = "Generation",
  #                   values = c("1" = "",
  #                              "2" = "lightgreen", 
  #                              "3" = "orange", 
  #                              "4" = "purple"),
  #                   labels = c("None", "A little", 
  #                              "Some", "A lot")) +
  theme_minimal()
