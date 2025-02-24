library(tidyverse)
library(readxl)
library(gt)
library(skedastic)
library(lmtest)
library(sandwich)
library(webshot2)

setwd("DIRETÓRIO")

basecnpj <- read_xlsx("Dados/basecnpj.xlsx")
basecpf <- read_xlsx("Dados/basecpf.xlsx")

basecnpj <- basecnpj %>% mutate(cpf = 0)
basecpf <- basecpf %>% mutate(cpf = 1)

basecpf$Data <- as.Date(basecpf$Data)
basecnpj$Data <- as.Date(basecnpj$Data)

basecnpj <- basecnpj %>% mutate(vol=vol/1000) %>% mutate(post = ifelse(Data >= as.Date("2023-07-01"),1, 0))
basecpf <- basecpf %>% mutate(vol=vol/1000) %>% mutate(post = ifelse(Data >= as.Date("2023-07-01"),1, 0))

basedid <- bind_rows(basecnpj, basecpf)
#------Estatísticas Descritivas-------------------------------------------------

summary(basecnpj)
summary(basecpf)

#Média
mean(basecpf$vol)
mean(basecnpj$vol)

media_comp_antes <- data.frame(
    
    variavel = c("Volume de Crédito", "Inadimplência", "Taxa de Juros"),
    media_cpf = c(
      mean(basecpf$vol[basecpf$post == 0]),
      mean(basecpf$inadim[basecpf$post == 0]),
      mean(basecpf$tdj[basecpf$post == 0])
      ),
    media_cnpj = c(
      mean(basecnpj$vol[basecnpj$post == 0]),
      mean(basecnpj$inadim[basecnpj$post == 0]),
      mean(basecnpj$tdj[basecnpj$post == 0])
    )
  )
medias_tabela_antes <- gt(media_comp_antes) %>%
    tab_header(
      title = "Comparação Entre as Médias (Pré Tratamento)"
    ) %>%
  cols_label(
    variavel = "Váriavel",
    media_cpf = "Médias (CPF)",
    media_cnpj = "Médias (CNPJ)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  fmt_number(
    columns = c(media_cpf, media_cnpj),
    decimals = 2
  )

gtsave(medias_tabela_antes, filename = "Comparação Médias (Pré Tratamento).png")

#Depois média
media_comp_depois <- data.frame(
  
  variavel = c("Volume de Crédito", "Inadimplência", "Taxa de Juros"),
  media_cpf = c(
    mean(basecpf$vol[basecpf$post == 1]),
    mean(basecpf$inadim[basecpf$post == 1]),
    mean(basecpf$tdj[basecpf$post == 1])
  ),
  media_cnpj = c(
    mean(basecnpj$vol[basecnpj$post == 1]),
    mean(basecnpj$inadim[basecnpj$post == 1]),
    mean(basecnpj$tdj[basecnpj$post == 1])
  )
)
medias_tabela_depois <- gt(media_comp_depois) %>%
  tab_header(
    title = "Comparação Entre as Médias (Pós Tratamento)"
  ) %>%
  cols_label(
    variavel = "Váriavel",
    media_cpf = "Médias (CPF)",
    media_cnpj = "Médias (CNPJ)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  fmt_number(
    columns = c(media_cpf, media_cnpj),
    decimals = 2
  )

gtsave(medias_tabela_depois, filename = "Comparação Médias (Pós Tratamento).png")

#Desvio Padrão
dp_comp_antes <- data.frame(
  
  variavel = c("Volume de Crédito", "Inadimplência", "Taxa de Juros"),
  dp_cpf = c(
    sd(basecpf$vol[basecpf$post == 0]),
    sd(basecpf$inadim[basecpf$post == 0]),
    sd(basecpf$tdj[basecpf$post == 0])
  ),
  dp_cnpj = c(
    sd(basecnpj$vol[basecnpj$post == 0]),
    sd(basecnpj$inadim[basecnpj$post == 0]),
    sd(basecnpj$tdj[basecnpj$post == 0])
  )
)
dp_tabela_antes <- gt(dp_comp_antes) %>%
  tab_header(
    title = "Comparação Entre Desvios Padrão (Pré Tratamento)"
  ) %>%
  cols_label(
    variavel = "Váriavel",
    dp_cpf = "Desvios Padrão (CPF)",
    dp_cnpj = "Desvios Padrão (CNPJ)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  fmt_number(
    columns = c(dp_cpf, dp_cnpj),
    decimals = 2
  )

gtsave(dp_tabela_antes, filename = "Comparação Desvios Padrão (Pré Tratamento).png")

#Depois dp
dp_comp_depois <- data.frame(
  
variavel = c("Volume de Crédito", "Inadimplência", "Taxa de Juros"),
dp_cpf = c(
  sd(basecpf$vol[basecpf$post == 1]),
  sd(basecpf$inadim[basecpf$post == 1]),
  sd(basecpf$tdj[basecpf$post == 1])
),
dp_cnpj = c(
  sd(basecnpj$vol[basecnpj$post == 1]),
  sd(basecnpj$inadim[basecnpj$post == 1]),
  sd(basecnpj$tdj[basecnpj$post == 1])
)
)
dp_tabela_depois <- gt(dp_comp_depois) %>%
  tab_header(
    title = "Comparação Entre Desvios Padrão (Pós Tratamento)"
  ) %>%
  cols_label(
    variavel = "Váriavel",
    dp_cpf = "Desvios Padrão (CPF)",
    dp_cnpj = "Desvios Padrão (CNPJ)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  fmt_number(
    columns = c(dp_cpf, dp_cnpj),
    decimals = 2
  )

gtsave(dp_tabela_depois, filename = "Comparação Desvios Padrão (Pós Tratamento).png")


#Gráficos Volume de crédito:
graf_volume_cpf <- ggplot(basecpf, aes(x = Data, y = vol)) +
  geom_line(color = "darkred") +
  geom_vline(xintercept = as.Date("2023-07-01"), color = "lightgrey", linetype = "dashed", linewidth = 0.8) +  
  geom_vline(xintercept = as.Date("2024-05-01"), color = "grey", linetype = "dashed", linewidth = 0.8) +  
    labs(title = "Volume de Crédito X Perído (CPF)", x = "Período", y = "Volume de Crédito") +
      theme_minimal()

ggsave("Volume Crédito CPF.png", plot = graf_volume_cpf, width = 6.5, height = 4.5, dpi = 300)      

graf_volume_cnpj <- ggplot(basecnpj, aes(x = Data, y = vol)) +
  geom_line(color = "red") +
  geom_vline(xintercept = as.Date("2023-07-01"), color = "lightgrey", linetype = "dashed", linewidth = 0.8) +  
  geom_vline(xintercept = as.Date("2024-05-01"), color = "grey", linetype = "dashed", linewidth = 0.8) +  
   labs(title = "Volume de Crédito X Perído (CNPJ)", x = "Período", y = "Volume de Crédito") +
    theme_minimal()
ggsave("Volume Crédito CNPJ.png", plot = graf_volume_cnpj, width = 6.5, height = 4.5, dpi = 300)

#Gráficos inadimplência:
graf_inad_cpf <- ggplot(basecpf, aes(x = Data, y = inadim)) +
  geom_line(color = "darkblue") +
  geom_vline(xintercept = as.Date("2023-07-01"), color = "lightgrey", linetype = "dashed", linewidth = 0.8) +  
  geom_vline(xintercept = as.Date("2024-05-01"), color = "grey", linetype = "dashed", linewidth = 0.8) +  
  labs(title = "Inadimplência X Perído (CPF)", x = "Período", y = "Inadimplência") +
  theme_minimal()
ggsave("Inadimplência CPF.png", plot = graf_inad_cpf, width = 6.5, height = 4.5, dpi = 300)

graf_inad_cnpj <- ggplot(basecnpj, aes(x = Data, y = inadim)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = as.Date("2023-07-01"), color = "lightgrey", linetype = "dashed", linewidth = 0.8) +  
  geom_vline(xintercept = as.Date("2024-05-01"), color = "grey", linetype = "dashed", linewidth = 0.8) +  
  labs(title = "Inadimplência X Perído (CNPJ)", x = "Período", y = "Inadimplência") +
  theme_minimal()
ggsave("Inadimplência CNPJ.png", plot = graf_inad_cnpj, width = 6.5, height = 4.5, dpi = 300)

#Gráficos Taxa de Juros:
graf_tdj_cpf <- ggplot(basecpf, aes(x = Data, y = tdj)) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as.Date("2023-07-01"), color = "lightgrey", linetype = "dashed", linewidth = 0.8) +  
  geom_vline(xintercept = as.Date("2024-05-01"), color = "grey", linetype = "dashed", linewidth = 0.8) +  
  labs(title = "Taxa de Juros X Perído (CPF)", x = "Período", y = "Taxa de Juros") +
  theme_minimal()
ggsave("Taxa de Juros CPF.png", plot = graf_tdj_cpf, width = 6.5, height = 4.5, dpi = 300)

graf_tdj_cnpj <- ggplot(basecnpj, aes(x = Data, y = tdj)) +
  geom_line(color = "green") +
  geom_vline(xintercept = as.Date("2023-07-01"), color = "lightgrey", linetype = "dashed", linewidth = 0.8) +  
  geom_vline(xintercept = as.Date("2024-05-01"), color = "grey", linetype = "dashed", linewidth = 0.8) +  
  labs(title = "Taxa de Juros X Perído (CNPJ)", x = "Período", y = "Taxa de Juros") +
  theme_minimal()
ggsave("Taxa de Juros CNPJ.png", plot = graf_tdj_cnpj, width = 6.5, height = 4.5, dpi = 300)

#-Regressão Diff-in-Diff--------------------------------------------------------

#Filtrar base para conter apenas a subamostra:
basedid <- basedid %>%
  filter(Data >= as.Date("2022-03-01"))

#Criar termo de interação:
basedid <- basedid %>%
  mutate(cpf_post = cpf * post)

#Rodar cada regressão (para vol, tdj e inad):

did_inadim <- lm(inadim ~ cpf + post + cpf_post, data = basedid)

did_vol <- lm(vol ~ cpf + post + cpf_post, data = basedid)

did_tdj <- lm(tdj ~ cpf + post + cpf_post, data = basedid)

summary(did_inadim)
summary(did_vol)
summary(did_tdj_df)

#Rodar testes de heteroscedasticidade:

white_inadim <- white(did_inadim, interactions = TRUE)

white_vol <- white(did_vol, interactions = TRUE)

white_tdj <- white(did_tdj, interactions = TRUE)

#Detectamos Heteroscedasticidade na taxa de juros. Usamos Erros Robustos Padrão:

did_tdj_robust <- coeftest(did_tdj, vcov. = vcovHC(did_tdj, type = "HC1"))

did_tdj_df <- did_tdj_robust[,] %>% 
  as_tibble() %>%
  mutate(variable = rownames(did_tdj_robust))

colnames(did_tdj_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
did_tdj_df <- did_tdj_df[, -5]
did_tdj_df <- as.data.frame(did_tdj_df)
rownames(did_tdj_df) <- c("(intercept)", "cpf", "post", "cpf_post")

#Criar tabelas para apresentar os resultado:

tabela_reg <- function(model, title) {
  
  coef_matrix <- as.data.frame(model)
  

  coef_matrix <- coef_matrix %>%
    mutate(
      Significance = case_when(
        `Pr(>|t|)` < 0.001 ~ "***",
        `Pr(>|t|)` < 0.01 ~ "**",
        `Pr(>|t|)` < 0.05 ~ "*",
        `Pr(>|t|)` < 0.1 ~ ".",
        TRUE ~ " "
      )
    )
  

  gt_table <- coef_matrix %>%
    gt(rownames_to_stub = TRUE) %>%
    fmt_number(columns = c(Estimate, `Std. Error`, `t value`, `Pr(>|t|)`), decimals = 3) %>%
    fmt_number(columns = `Pr(>|t|)`, decimals = 3) %>%
    cols_label(
      Estimate = "Estimativa",
      `Std. Error` = "Erro Padrão",
      `t value` = "Estatística t",
      `Pr(>|t|)` = "p-valor",
      Significance = "Significância"
    ) %>%
    tab_header(title = title)
  
  return(gt_table)
}

did_inadim_table <- tabela_reg(summary(did_inadim)$coefficients, "Inadimplência")
did_vol_table <- tabela_reg(summary(did_vol)$coefficients, "Volume")
did_tdj_table <- tabela_reg(did_tdj_df, "Taxa de Juros")

# Mostrar tabelas
did_inadim_table
did_vol_table
did_tdj_table

#Salvar tabelas:

# Save did_inadim_table
gtsave(
  did_inadim_table,
  filename = "Diff-in-Diff Inadimplência.png",
  zoom = 2,  
  expand = 10,  
  vwidth = 1000,  
  vheight = 600   
)

# Save did_vol_table
gtsave(
  did_vol_table,
  filename = "Diff-in-Diff Volume.png",
  zoom = 2,
  expand = 10,
  vwidth = 1000,
  vheight = 600
)

# Save did_tdj_table
gtsave(
  did_tdj_table,
  filename = "Diff_in_Diff TDJ.png",
  zoom = 2,
  expand = 10,
  vwidth = 1000,
  vheight = 600
)

#Checar possível "spillover" na taxa de juros:

controle <- basedid %>% filter(cpf == 0)

controle %>%
  group_by(post) %>%
  summarise(mean_tdj = mean(tdj, na.rm = TRUE))

t.test(tdj ~ post, data = controle) #ISSO INDICA SPILLOVER!







