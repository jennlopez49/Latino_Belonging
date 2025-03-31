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
