# analystportfolio
American University, Course SIS-750

# Introduction
This repository represents the culmination of my work in the graduate "Data Analysis" course at American University. The assignment was to include a resume tailored to a data analyst position, an exemplary code chunk from an assignment earlier in the semester, and two samples of improved original work from earlier in the semester.

For the code chunk, I used a chunk that generates a graph meant to replicate the graph found here: https://www.natesilver.net/p/trump-approval-ratings-nate-silver-bulletin. It demonstrates many different skills on R, specifically using the ggplot2 package.

For the first sample of original work, I included an analysis of regional conflict trends taken from the UCDP dataset found here: https://ucdp.uu.se/. It includes a written portion, summary statistics, and a visual component. This is presented with a Quarto PDF document.

Finally, for the second sample of original work, I used my analysis of women's legal and economic rights improving over time using the World Bank's report on Women, Business, and the Law dataset found here: https://wbl.worldbank.org/en/data/download-data#tabs-1c6624179e-item-d6630763cb-tab. This sample includes several graphs, summary statistics, and regression analysis, presented with a Quarto Beamer presentation.

# Sample Code Chunk
This contains my sample code chunk pulled from an assignment earlier in the semester. It generates a line graph that is meant to replicate the graph from this link as best as possible: https://www.natesilver.net/p/trump-approval-ratings-nate-silver-bulletin.

Note: The graph was meant to replicate the plot taken from this website several months ago, in mid-February 2026.

```{r}
ggplot(trump, aes(x = model_date, y = approve)) +
  geom_ribbon(aes(ymin = approve_lo, ymax = approve_hi), fill = "#3B9C9C", alpha = 0.3, linetype = "dotted") +
  geom_line(aes(y = approve), color = "#3B9C9C", linewidth = 0.9) +
  geom_ribbon(aes(ymin = disapprove_lo, ymax = disapprove_hi), fill = "#F87217", alpha = 0.3, linetype = "dotted") +
  geom_line(aes(y = disapprove), color = "#F87217", linewidth = 0.9) +
  geom_line(aes(y = approve_hi), color = "#3B9C9C", linetype = "dotted") +
  geom_line(aes(y = approve_lo), color = "#3B9C9C", linetype = "dotted") +
  geom_line(aes(y = disapprove_hi), color = "#F87217", linetype = "dotted") +
  geom_line(aes(y = disapprove_lo), color = "#F87217", linetype = "dotted") +
  scale_color_manual(values = c("approve" = "#3B9C9C", "disapprove" = "#F87217")) +
  geom_hline(yintercept = 50, linetype = "solid", color = "black", linewidth = 0.5) +
  scale_y_continuous(
    limits = c(30, 70),
    labels = label_number(accuracy = 0.1, suffix = "%"),
    expand = expansion(mult = c(0, .1))
  ) + 
  scale_x_date(
    breaks = "3 months",
    labels = label_date(format = "%b"),
    limits = c(ymd("2025-01-21"), ymd("2026-02-11")),
    expand = expansion(mult = c(0, .1)) 
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = 'Approval',
    title = "Do Americans approve or disapprove of Donald Trump?",
    subtitle = "An updating polling average of Donald Trump's approval rating and disapproval rating in his second term, accounting for each poll's quality, recency, sample size, and partisan lean",
    caption = 'SILVER\nBULLETIN'
  ) +
  theme(
      panel.grid.major = element_line(color = "lightgrey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(t = 10, r = 30, b = 10, l = 10),
      plot.title = element_text(face = "bold", size = 18),
      plot.caption = element_text(face = "bold"),
    ) +
  geom_text(data = label_data, 
            aes(x = model_date, y = disapprove),
            label = "Disapprove\n55.5%", color = "#f87217", hjust = -0.1) +
  geom_text(data = label_data, 
            aes(x = model_date, y = approve),
            label = "Approve\n41.1%", color = "#3b9c96", hjust = -0.1) +
  coord_cartesian(clip = "off")
```

## Output of Code Chunk
<img width="2375" height="1174" alt="Sample Code Chunk" src="https://github.com/user-attachments/assets/5b198fca-5524-4f80-9267-acf414734b6a" />

# 1st Sample of Original Work
This is a study of regional conflict trends using data from UCDP from an earlier assignment. I have attached both the Quarto PDF Document here, and the replication .qmd file can be found in my repository.

[1st Sample of Original Work.pdf](https://github.com/user-attachments/files/27453568/1st.Sample.of.Original.Work.pdf)

# 2nd Sample of Original Work
This is a study of women's legal and economic rights using data taken from the World Bank. I have attached the Quarto Beamer Presentation, and the replication .qmd file can be found in my repository.

[2nd Sample of Original Work.pdf](https://github.com/user-attachments/files/27453623/2nd.Sample.of.Original.Work.pdf)

# Analyst Resume
Finally, this is my resume specifically tailored to a data analyst position.

[Emily Combs Resume.docx](https://github.com/user-attachments/files/27453629/Emily.Combs.Resume.docx)

