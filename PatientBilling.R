library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

setwd('C:/Users/hanna/Documents/DATA 332/PatientBilling')
df_billing <- read_excel('Billing.xlsx', .name_repair = 'universal')
df_patient <- read_excel('Patient.xlsx', .name_repair = 'universal')
df_visit <- read_excel('Visit.xlsx', .name_repair = 'universal')

df <- left_join(df_billing, df_visit, by = c('VisitID'))
df <- left_join(df, df_patient, by = c('PatientID'))

df$VisitDate <- as.Date(df$VisitDate)
df$VisitYear <- year(df$VisitDate)
df$VisitMonth <- month(df$VisitDate)

df$ReasonSimple <- sapply(strsplit(as.character(df$Reason), " "), function(x) x[1])

df_reason_for_visit <- df %>%
  group_by(ReasonSimple,VisitMonth, VisitYear) %>%
  summarize(count = n())

ggplot(df_reason_for_visit, aes(x = VisitYear, y = ReasonSimple, fill = factor(VisitMonth))) +
  geom_col() +
  labs(title = "Time of Visit",
       x = "Year",
       y = "Reason",
       fill = "Month") +
  theme(axis.text = element_text(angle = 0, vjust = .5, hjust = .5))

df_reason_for_visit_walkin_chart <- df %>%
  group_by(ReasonSimple, WalkIn) %>%
  summarize(count = n())

ggplot(df_reason_for_visit_walkin_chart, aes(x = count, y = ReasonSimple, fill = WalkIn)) +
  geom_col() +
  labs(title = "Reason For Walk-In Visit",
       x = "Count",
       y = "Reason",
       fill = "Walk-In") +
  theme(axis.text = element_text(angle = 0, vjust = .5, hjust = .5))

df_reason_for_visit_location_chart <- df %>%
  group_by(ReasonSimple,City,State) %>%
  summarize(count = n())

ggplot(df_reason_for_visit_location_chart, aes(x = count, y = ReasonSimple, fill = City)) +
  geom_col() +
  labs(title = "Location of Reason for Visit",
       x = "Count",
       y = "Reason",
       fill = "City") +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = .5)) +
  theme(axis.text.y = element_text(angle = 0, vjust = .5, hjust = .5))

df_total_invoice_amount_chart <- df %>%
  group_by(ReasonSimple, InvoicePaid) %>%
  summarize(total_amount = sum(InvoiceAmt))

ggplot(df_total_invoice_amount_chart , aes(x = total_amount, y = ReasonSimple, fill = InvoicePaid)) +
  geom_col() +
  labs(title = "Total Invoice Amount",
       x = "Cost",
       y = "Reason",
       fill = "Paid") +
  theme(axis.text = element_text(angle = 0, vjust = .5, hjust = .5))
  
df_walkin_date <- df %>%
  group_by(ReasonSimple, InvoiceItem) %>%
  summarize(count = n())

ggplot(df_walkin_date, aes(x = count, y = ReasonSimple, fill = InvoiceItem)) +
  geom_col() +
  labs(title = "Reason Type",
       x = "Count",
       y = "Reason",
       fill = "Type")
  scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text = element_text(angle = 0, vjust = .5, hjust = .5))



