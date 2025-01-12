# Fungsi untuk ANOVA dua arah
uji_dua_arah <- function(data, response, factor1, factor2) {
  # Validasi input
  if (!all(c(response, factor1, factor2) %in% colnames(data))) {
    stop("Kolom tidak ditemukan dalam data.")
  }

  # Formula model
  formula <- as.formula(paste(response, "~", factor1, "*", factor2))

  # Analisis ANOVA
  hasil <- aov(formula, data = data)

  # Kembalikan hasil
  summary(hasil)
}
#' Uji ANOVA Dua Arah
#'
#' Fungsi ini melakukan analisis ANOVA dua arah pada data tertentu.
#'
#' @param data Data frame yang mengandung variabel.
#' @param response Nama kolom respons (variabel dependen).
#' @param factor1 Nama kolom faktor pertama (variabel independen).
#' @param factor2 Nama kolom faktor kedua (variabel independen).
#' @return Ringkasan hasil ANOVA dua arah.
#' @examples
#' @export
# Contoh penggunaan:
# data <- data.frame(y = rnorm(30), A = rep(letters[1:3], each = 10), B = rep(LETTERS[1:2], times = 15))
# uji_dua_arah(data, "y", "A", "B")
