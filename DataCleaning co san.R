###########################################################################
# ĐỀ TÀI: LÀM SẠCH VÀ TRỰC QUAN HÓA DỮ LIỆU LAPTOP
# Dựa trên quy trình file: 10-DataCleaning.R
###########################################################################

# =========================================================================
# BƯỚC 1: CÀI ĐẶT VÀ LOAD THƯ VIỆN
# =========================================================================

# Danh sách các thư viện cần thiết
packages <- c("tidyverse", "janitor", "stringr", "ggplot2", "corrplot", "scales")

# Kiểm tra và cài đặt tự động nếu thiếu
for (p in packages) {
  if (!require(p, character_only = TRUE)) {
    install.packages(p)
    library(p, character_only = TRUE)
  }
}

# =========================================================================
# BƯỚC 2: LOAD VÀ KHÁM PHÁ DỮ LIỆU THÔ (EXPLORATORY DATA ANALYSIS)
# =========================================================================

# 2.1. Đọc dữ liệu từ file laptops.csv 
laptops_raw <- read.csv("laptops.csv", stringsAsFactors = FALSE, check.names = FALSE)

# 2.2. Xem cấu trúc ban đầu
print("--- Cấu trúc dữ liệu ban đầu ---")
str(laptops_raw)
head(laptops_raw)

# 2.3. Kiểm tra dữ liệu thiếu (Missing values)
# Tương tự bước xử lý missing data trong 10-DataCleaning.R
col_na_count <- sapply(laptops_raw, function(x) sum(is.na(x) | x == ""))
print("--- Số lượng giá trị trống trong mỗi cột ---")
print(col_na_count)

# =========================================================================
# BƯỚC 3: LÀM SẠCH TÊN CỘT VÀ LOẠI BỎ DỮ LIỆU THỪA
# =========================================================================

laptops_clean <- laptops_raw %>%
  # Chuyển tên cột về dạng snake_case (ví dụ: ScreenResolution -> screen_resolution)
  clean_names() %>%
  # Loại bỏ cột đầu tiên (ID/X) nếu nó không mang ý nghĩa phân tích
  select(-1) 

# =========================================================================
# BƯỚC 4: XỬ LÝ KIỂU DỮ LIỆU (DATA TRANSFORMATION)
# Đây là bước quan trọng nhất vì RAM và Weight đang ở dạng text "8GB", "1.3kg" 
# =========================================================================

laptops_processed <- laptops_clean %>%
  mutate(
    # 4.1. Làm sạch cột Ram: Bỏ "GB" và chuyển sang số (Numeric)
    ram_gb = as.numeric(str_replace_all(ram, "GB", "")),
    
    # 4.2. Làm sạch cột Weight: Bỏ "kg" và chuyển sang số (Numeric)
    weight_kg = as.numeric(str_replace_all(weight, "kg", "")),
    
    # 4.3. Trích xuất tốc độ CPU (GHz) từ cột cpu
    cpu_speed_ghz = as.numeric(str_extract(cpu, "[0-9.]+(?=GHz)")),
    
    # 4.4. Phân loại hãng CPU (Intel, AMD, Samsung)
    cpu_brand = word(cpu, 1),
    
    # 4.5. Xử lý độ phân giải màn hình
    resolution = str_extract(screen_resolution, "\\d+x\\d+"),
    screen_width = as.numeric(str_extract(resolution, "^\\d+")),
    screen_height = as.numeric(str_extract(resolution, "\\d+$")),
    
    # 4.6. Tạo biến logic: Có phải màn hình cảm ứng (Touchscreen) không?
    is_touchscreen = ifelse(str_detect(screen_resolution, "Touchscreen"), "Yes", "No"),
    
    # 4.7. Tạo biến logic: Có tấm nền IPS không? 
    is_ips = ifelse(str_detect(screen_resolution, "IPS"), "Yes", "No")
  )

# 4.8. Phân loại ổ cứng (Memory) - Rất phức tạp vì có thể có ổ kép (SSD + HDD)
laptops_processed <- laptops_processed %>%
  mutate(
    has_ssd = str_detect(memory, "SSD"),
    has_hdd = str_detect(memory, "HDD"),
    has_flash = str_detect(memory, "Flash Storage")
  )

# 4.9. Chuyển đổi các biến định danh (Categorical) thành Factor
# Giống như cách xử lý biến 'health' và 'alcohol' trong file mẫu của bạn
category_vars <- c("company", "type_name", "cpu_brand", "is_touchscreen", "is_ips", "op_sys")
laptops_processed[category_vars] <- lapply(laptops_processed[category_vars], factor)

# =========================================================================
# BƯỚC 5: TRỰC QUAN HÓA DỮ LIỆU (DATA VISUALIZATION)
# =========================================================================

# 5.1. Phân phối giá Laptop theo thương hiệu
# Giúp nhận diện phân khúc thị trường
ggplot(laptops_processed, aes(x = reorder(company, price_euros, median), y = price_euros, fill = company)) +
  geom_boxplot(alpha = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = unit_format(unit = "€")) +
  labs(title = "Phân phối giá Laptop theo thương hiệu",
       subtitle = "Sắp xếp theo trung vị giá từ cao xuống thấp",
       x = "Thương hiệu", y = "Giá (Euros)") +
  theme_minimal() + theme(legend.position = "none")

# 5.2. Mối quan hệ giữa Trọng lượng và Kích thước màn hình
# Kiểm tra xem máy càng to thì có thực sự càng nặng không?
ggplot(laptops_processed, aes(x = inches, y = weight_kg, color = type_name)) +
  geom_jitter(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(title = "Mối tương quan giữa Kích thước (Inches) và Cân nặng (Kg)",
       x = "Kích thước màn hình", y = "Cân nặng",
       color = "Loại máy") +
  theme_bw()

# 5.3. Biểu đồ cột: Số lượng Laptop theo hệ điều hành và Cảm ứng
ggplot(laptops_processed, aes(x = op_sys, fill = is_touchscreen)) +
  geom_bar(position = "dodge") +
  labs(title = "Thống kê Laptop có màn hình cảm ứng theo Hệ điều hành",
       x = "Hệ điều hành", y = "Số lượng",
       fill = "Cảm ứng") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.4. Tương quan giữa RAM và Giá theo dòng máy
ggplot(laptops_processed, aes(x = factor(ram_gb), y = price_euros, fill = type_name)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  labs(title = "Ảnh hưởng của dung lượng RAM đến giá thành",
       x = "RAM (GB)", y = "Giá (Euros)",
       fill = "Dòng máy") +
  theme_minimal()

# =========================================================================
# BƯỚC 6: PHÂN TÍCH THỐNG KÊ TÓM TẮT
# =========================================================================

# Tạo bảng tóm tắt giá trị trung bình theo từng hãng
company_summary <- laptops_processed %>%
  group_by(company) %>%
  summarise(
    so_luong = n(),
    gia_trung_binh = mean(price_euros),
    ram_trung_binh = mean(ram_gb),
    can_nang_nhe_nhat = min(weight_kg)
  ) %>%
  arrange(desc(gia_trung_binh))

print("--- Bảng tóm tắt theo thương hiệu ---")
print(company_summary)

# =========================================================================
# BƯỚC 7: LƯU DỮ LIỆU ĐÃ LÀM SẠCH
# =========================================================================

# Lưu kết quả ra file mới để sử dụng cho các mô hình Machine Learning sau này
write.csv(laptops_processed, "laptops_cleaned_final.csv", row.names = FALSE)

print("HOÀN THÀNH QUY TRÌNH LÀM SẠCH DỮ LIỆU!")