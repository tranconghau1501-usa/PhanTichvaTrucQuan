###########################################################################
# ĐỀ TÀI: LÀM SẠCH VÀ TRỰC QUAN HÓA TOP 100 SPOTIFY ALL-TIME
# Dựa trên tiêu chuẩn file: 10-DataCleaning.R
###########################################################################

# =========================================================================
# BƯỚC 1: LOAD THƯ VIỆN VÀ THIẾT LẬP MÔI TRƯỜNG
# =========================================================================

# Sử dụng pacman để quản lý thư viện hiệu quả
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, ggplot2, corrplot, ggthemes, scales, viridis)

# =========================================================================
# BƯỚC 2: KHÁM PHÁ DỮ LIỆU BAN ĐẦU (EDA)
# =========================================================================

# 2.1. Đọc dữ liệu
spotify_raw <- read.csv("spotify_alltime_top100_songs.csv", stringsAsFactors = FALSE)

# 2.2. Kiểm tra tổng quan
print("--- Cấu trúc dữ liệu Spotify ---")
glimpse(spotify_raw)

# 2.3. Kiểm tra dữ liệu thiếu (Sử dụng logic từ file 10-DataCleaning)
missing_values <- colSums(is.na(spotify_raw))
print("--- Thống kê giá trị thiếu ---")
print(missing_values)

# =========================================================================
# BƯỚC 3: LÀM SẠCH VÀ CHUẨN HÓA (CLEANING)
# =========================================================================

spotify_clean <- spotify_raw %>%
  # Chuyển tên cột sang dạng chuẩn (snake_case)
  clean_names() %>%
  # Xử lý dữ liệu trùng lặp nếu có
  distinct() %>%
  # Xử lý cột explicit (Logic: Chuyển sang Factor giống file mẫu)
  mutate(
    explicit = factor(explicit, levels = c("True", "False"), labels = c("Yes", "No")),
    release_year = as.numeric(release_year)
  )

# 3.1. Phân nhóm các bài hát theo thập kỷ (Feature Engineering)
spotify_clean <- spotify_clean %>%
  mutate(decade = paste0(floor(release_year / 10) * 10, "s"))

# 3.2. Chuyển đổi các biến phân loại thành Factor có thứ tự
# Tương tự như cách file mẫu xử lý biến 'health'
spotify_clean$primary_genre <- as.factor(spotify_clean$primary_genre)
spotify_clean$artist_country <- as.factor(spotify_clean$artist_country)

# =========================================================================
# BƯỚC 4: TRỰC QUAN HÓA DỮ LIỆU (VISUALIZATION)
# =========================================================================

# 4.1. Biểu đồ Top 10 Nghệ sĩ có nhiều bài hát nhất trong Top 100
top_artists <- spotify_clean %>%
  count(artist, sort = TRUE) %>%
  head(10)

ggplot(top_artists, aes(x = reorder(artist, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "magma") +
  labs(
    title = "Top 10 Nghệ sĩ quyền lực nhất Spotify All-Time",
    subtitle = "Dựa trên số lượng bài hát lọt vào Top 100",
    x = "Nghệ sĩ", y = "Số lượng bài hát"
  ) +
  theme_minimal()

# 4.2. Mối quan hệ giữa Năng lượng (Energy) và Độ sôi động (Danceability)
# Đây là phân tích đặc trưng của dữ liệu âm nhạc
ggplot(spotify_clean, aes(x = energy, y = danceability, color = explicit)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(
    title = "Sự tương quan giữa Energy và Danceability",
    x = "Mức độ năng lượng", y = "Độ dễ nhảy",
    color = "Gắn mác Explicit"
  ) +
  theme_hc()

# 4.3. Phân phối dòng nhạc (Genre) theo tổng lượt Stream
genre_streams <- spotify_clean %>%
  group_by(primary_genre) %>%
  summarise(total_billions = sum(total_streams_billions)) %>%
  arrange(desc(total_billions))

ggplot(genre_streams, aes(x = reorder(primary_genre, total_billions), y = total_billions)) +
  geom_segment(aes(xend = primary_genre, yend = 0), color = "skyblue") +
  geom_point(size = 4, color = "blue") +
  coord_flip() +
  labs(
    title = "Dòng nhạc nào thống trị lượt Stream?",
    x = "Thể loại", y = "Tổng lượt Stream (Tỷ lượt)"
  ) +
  theme_light()

# 4.4. Ma trận tương quan (Correlation Matrix) giữa các chỉ số âm thanh
# Tương tự bước phân tích đa biến trong file 10-DataCleaning
audio_features <- spotify_clean %>%
  select(danceability, energy, valence, acousticness, bpm)

cor_matrix <- cor(audio_features)
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "\nTương quan giữa các thuộc tính âm thanh")

# =========================================================================
# BƯỚC 5: PHÂN TÍCH SÂU (DEEP DIVE)
# =========================================================================

# 5.1. Thống kê theo quốc gia của nghệ sĩ
country_stats <- spotify_clean %>%
  group_by(artist_country) %>%
  summarise(
    song_count = n(),
    avg_streams = mean(total_streams_billions),
    avg_bpm = mean(bpm)
  ) %>%
  arrange(desc(song_count))

print("--- Thống kê theo quốc gia ---")
print(country_stats)

# 5.2. Tìm bài hát "Tích cực" nhất (Valence cao nhất)
positive_songs <- spotify_clean %>%
  select(song_title, artist, valence) %>%
  arrange(desc(valence)) %>%
  head(5)

print("--- Top 5 bài hát mang giai điệu tích cực nhất ---")
print(positive_songs)

# =========================================================================
# BƯỚC 6: KIỂM TRA VÀ LƯU TRỮ
# =========================================================================

# Kiểm tra lại lần cuối xem có dòng nào thiếu dữ liệu không (Complete Cases)
if(sum(!complete.cases(spotify_clean)) == 0) {
  print("Dữ liệu đã hoàn toàn sạch sẽ!")
}

# Lưu file kết quả
write.csv(spotify_clean, "spotify_top100_cleaned.csv", row.names = FALSE)

# =========================================================================
# KẾT THÚC QUY TRÌNH
# =========================================================================