
# Fuzzy Time Series Stevenson Porter

![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)

### Authors
- [@clavinorach](https://www.linkedin.com/in/clavinorachmadi)

## Deskripsi
Proyek ini bertujuan untuk memprediksi suhu rata-rata harian di Kabupaten Kolaka, Sulawesi Tenggara menggunakan metode Fuzzy Time Series Stevenson Porter. Analisis ini dilakukan dengan menggunakan data dari BMKG untuk periode tahun 2001 hingga 2024. Selain itu, kami juga mengembangkan aplikasi web interaktif menggunakan Shiny untuk memvisualisasikan hasil prediksi.

## Fitur
- Prediksi suhu rata-rata harian menggunakan metode Fuzzy Time Series Stevenson Porter.
- Analisis data BMKG dari tahun 2001 hingga 2024.
- Aplikasi web interaktif menggunakan Shiny untuk visualisasi prediksi.
- Kemampuan untuk menggunakan data lain dalam aplikasi web.
- Dukungan untuk metode Fuzzy Time Series lainnya, termasuk Markov Chain.

## Instalasi
1. Clone repositori ini:
   ```sh
   git clone https://github.com/clavinorach/fuzzy-time-series-stevenson-porter.git
   cd fuzzy-time-series-stevenson-porter
2. Instal paket R yang diperlukan:
   ```sh
   install.packages(c("shiny", "dplyr", "ggplot2", "lubridate"))
3. Jalankan aplikasi Shiny:
   ```sh
   library(shiny)
    runApp('path/to/your/app')  # Pastikan untuk mengganti 'path/to/your/app' dengan  path ke direktori aplikasi Shiny Anda

## Penggunaan
Setelah menginstal semua paket yang diperlukan dan menjalankan aplikasi Shiny, Anda dapat mengakses aplikasi melalui browser web Anda. Aplikasi ini memungkinkan Anda untuk:

- Melihat prediksi suhu rata-rata harian di Kabupaten Kolaka.
- Mengunggah dan menggunakan data lain untuk analisis.
- Memilih dan menerapkan metode Fuzzy Time Series lainnya, seperti Markov Chain, untuk prediksi.

**RPubs**: https://rpubs.com/clavinorach/1203735
**Sumber Data:** https://dataonline.bmkg.go.id/
