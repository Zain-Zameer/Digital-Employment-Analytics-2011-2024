library("readr")
library("dplyr")
library("ggplot2")

# Loading dataframe
df <- read_csv("jobs_01.csv")

# Trend Analysis
 # For each job role
job_df <- df %>%
group_by(Year,`Job Type`) %>%
  summarise(Total=sum(`Estimated Number`))

year_2011_DataAnalyst <- job_df %>%
  filter(`Job Type`=="Data Analyst")
ggplot(data=year_2011_DataAnalyst,aes(x=Year,y=Total)) +
  geom_line()+ geom_point() +
  scale_x_continuous(breaks=seq(2011, 2024, by=1)) +
  theme_minimal() +
  labs(title="Data Analyst Analysis",subtitle="Analysis from 2011 - 2024",x="Years",y="Estimated Numbers")

 # regions wise
growth_rate_data <- df %>%
  group_by(Region,`Job Type`) %>%
  mutate(Growth_Rate = (`Estimated Number`-lag(`Estimated Number`))/lag(`Estimated Number`))

regions_wise <- growth_rate_data %>%
  filter(Region=="Europe",`Job Type`=="Data Entry Specialist")

ggplot(data=regions_wise,aes(x=Year,y=Growth_Rate)) +
  geom_line()+ geom_point() +
  scale_x_continuous(breaks=seq(2011, 2024, by=1)) +
  theme_minimal() + 
  labs(title="Data Entry Specialist Analysis",subtitle="Analysis from 2011 - 2024 in Europe",x="Years",y="Growth Rate") +
  scale_y_continuous(labels=scales::percent)



# Regional Distribution
regional <- growth_rate_data %>%
  filter(Year==max(Year)) %>%
  group_by(Region) %>%
  summarise(Total=sum(`Estimated Number`))

ggplot(data=regional,aes(x=reorder(Region, -Total),y=Total))+
  geom_bar(stat="identity",aes(fill=Region))+
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  labs(x="Regions",y="Total Estimated Jobs",title="Job Analysis 2024",subtitle="Analysis on total number of estimated jobs in each region 2024")


# Role Popularity
role_popular <- df %>%
  group_by(`Job Type`) %>%
  summarise(Total=sum(`Estimated Number`))

ggplot(data=role_popular,aes(x=reorder(`Job Type`, -Total),y=Total)) +
  geom_bar(stat="identity",aes(fill=`Job Type`)) +
  theme_minimal() + 
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  coord_flip() +
  labs(x="Job Roles",y="Total Estimated Jobs",title="IT Jobs Analysis",subtitle="Analysis on total number of estimated jobs for each role.")


# Yearly Comparison
yearly_Comparison <- growth_rate_data %>%
  group_by(Year) %>%
  summarise(Total=sum(`Estimated Number`))

ggplot(data=yearly_Comparison,aes(x=reorder(Year,Total),y=Total)) +
  geom_bar(stat="identity",fill="lightgreen") +
  theme_minimal() + 
  theme(axis.text.x = element_blank()) +
  coord_flip() +
  labs(x="Years", y="Total IT Jobs",title="IT Jobs Analysis",subtitle="Year wise analysis 2011-2024")
  
