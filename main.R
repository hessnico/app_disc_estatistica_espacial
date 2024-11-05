setwd('./')

paths = 'downloaded'
if (!dir.exists(paths)) {
  dir.create(paths, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

source('./utils_est_espacial.R')
cat(sprintf("At:"), getwd())

df_mun001 <- main(ano = 2001)
df_mun001$Year = 2001
df_mun002 <- main(ano = 2002)
df_mun002$Year = 2002
df_mun003 <- main(ano = 2003)
df_mun003$Year = 2003
df_mun004 <- main(ano = 2004)
df_mun004$Year = 2004
df_mun005 <- main(ano = 2005)
df_mun005$Year = 2005
df_mun006 <- main(ano = 2006)
df_mun006$Year = 2006

df_mun_inteiro <- rbind(df_mun001,
                        df_mun002,
                        df_mun003,
                        df_mun004,
                        df_mun005,
                        df_mun006)
