# Nombre: Factores que influyen en la depresión de estudiantes
# Autores:  Dixon Montero Hernández, Jeremy Flores Bonilla, Josep Romero Chinchilla
# Fecha: 07-05-2025

# Librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(janitor)

# Carga de los datos
df <- read_excel("data/depresion_estudiantil.xlsx")

# Vista general de los datos
str(df)
summary(df)

# Limpieza de nombres de columnas
df <- clean_names(df)

# Conversión de variables categoricas a factor
df <- df %>% mutate(
  gender <- as.factor(gender),
  city <- as.factor(city),
  profession <- as.factor(profession),
  sleep_duration <- as.factor(sleep_duration),
  dietary_habits = as.factor(dietary_habits),
  degree = as.factor(degree),
  suicidal_thoughts = as.factor(`have_you_ever_had_suicidal_thoughts`),
  family_history = as.factor(family_history_of_mental_illness),
  depression = as.factor(depression)
)

# Frecuencias básicas
table(df$depression)
prop.table(table(df$depression))

# Gráficos categóricos
categorical_vars <- c("gender", "dietary_habits", "sleep_duration", 
                      "suicidal_thoughts", "family_history", "degree")


# Gráficos categóricos

# Sexo
ggplot(df, aes_string(x = "gender", fill = "depression")) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de sexo por Depresión",
       x = "Sexo",
       y = "Proporción", 
       fill = "Depresión") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Hábitos alimentarios
ggplot(df, aes_string(x = "dietary_habits", fill = "depression")) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de hábitos alimentarios \npor Depresión",
       x = "Hábitos alimentarios",
       y = "Proporción", 
       fill = "Depresión") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Duración de sueño
ggplot(df, aes_string(x = "sleep_duration", fill = "depression")) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de duración de sueño \npor Depresión",
       x = "Duración",
       y = "Proporción", 
       fill = "Depresión") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Pensamientos suicidas
ggplot(df, aes_string(x = "suicidal_thoughts", fill = "depression")) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de la depresión \nen función de los pensamientos suicidas.",
       x = "Pensamietos suicidas",
       y = "Proporción", 
       fill = "Depresión") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Historial familiar
ggplot(df, aes_string(x = "family_history", fill = "depression")) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de la depresión \nen función del historial familiar.",
       x = "Historial familiar",
       y = "Proporción", 
       fill = "Depresión") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Grado academico
ggplot(df, aes_string(x = "degree", fill = "depression")) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de la depresión \nen función del grado educativo.",
       x = "Grado",
       y = "Proporción", 
       fill = "Depresión") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
  coord_flip()

# Gráficos númericos

# Edad
ggplot(df, aes_string(x = "depression", y = "age", fill = "depression")) +
  geom_boxplot() +
  labs(title = paste("Boxplot de depresión por edad")) +
  theme_minimal()

# Presión academica
ggplot(df, aes_string(x = "depression", y = "academic_pressure", fill = "depression")) +
  geom_boxplot() +
  labs(title = paste("Boxplot de depresión por presión academica")) +
  theme_minimal()

# Presión laboral
ggplot(df, aes_string(x = "depression", y = "work_pressure", fill = "depression")) +
  geom_boxplot() +
  labs(title = paste("Boxplot de depresión por presión laboral")) +
  theme_minimal()

# CGPA
ggplot(df, aes_string(x = "depression", y = "cgpa", fill = "depression")) +
  geom_boxplot() +
  labs(title = paste("Boxplot de depresión por promedio general de notas")) +
  theme_minimal()

# Satisfacción estudiantil
ggplot(df, aes_string(x = "depression", y = "study_satisfaction", fill = "depression")) +
  geom_boxplot() +
  labs(title = paste("Boxplot de depresión por satisfacción estudiantil")) +
  theme_minimal()

# Horas de estudio y trabajo
ggplot(df, aes_string(x = "depression", y = "work_study_hours", fill = "depression")) +
  geom_boxplot() +
  labs(title = "Boxplot de depresión por horas \nde estudio y trabajo") +
  theme_minimal()

# Estrés financiero
ggplot(df, aes_string(x = "depression", y = "financial_stress", fill = "depression")) +
  geom_boxplot() +
  labs(title = "Boxplot de depresión por estrés financiero") +
  theme_minimal()
