# ================================================================
# PHAN TICH DU LIEU HFPS VIETNAM 2022
# Nhom 10 | Lop 26D1MAT50801003 | Mon Phan tich Du lieu
# Giang vien: ThS. Tran Ha Quyen
# ================================================================

# ================================================================
# PHAN 0: SETUP
# ================================================================
pkgs <- c("nortest","car","ggplot2","dplyr","dunn.test",
          "corrplot","pROC","DescTools","ResourceSelection","scales","tidyr")
install.packages(pkgs[!pkgs %in% installed.packages()[,"Package"]])

library(nortest); library(car); library(ggplot2); library(dplyr)
library(dunn.test); library(corrplot); library(pROC)
library(DescTools); library(ResourceSelection); library(scales); library(tidyr)

theme_set(theme_minimal(base_size = 12))
my_cols <- c("#2196F3","#F44336")

# SUA DUONG DAN PHU HOP VOI MAY CUA BAN
df <- read.csv("C:/Users/admin/Downloads/hfps_final_v3.csv")
cat("Data loaded:", nrow(df), "obs |", ncol(df), "vars\n")

# Factor variables
df$income_shock_f    <- factor(df$income_shock,    levels=c(0,1), labels=c("Khong cu soc","Co cu soc"))
df$cut_consumption_f <- factor(df$cut_consumption, levels=c(0,1), labels=c("Khong cat","Co cat"))
df$low_resilience_f  <- factor(df$low_resilience,  levels=c(0,1), labels=c("Tiet kiem du","Tiet kiem thap"))
df$has_child_f       <- factor(df$has_child,        levels=c(0,1), labels=c("Khong co con","Co con"))
df$employed_f        <- factor(df$employed,         levels=c(0,1), labels=c("Khong viec lam","Co viec lam"))
df$male_f            <- factor(df$male,             levels=c(0,1), labels=c("Nu","Nam"))
df$renter_f          <- factor(df$renter,           levels=c(0,1), labels=c("So huu nha","Thue nha"))
df$covid_recent_f    <- factor(df$covid_recent,     levels=c(0,1), labels=c("Khong nhiem","Nhiem gan day"))
df$ethnic_kinh_f     <- factor(df$ethnic_kinh,      levels=c(0,1), labels=c("Dan toc thieu so","Kinh"))
df$region_f          <- factor(df$region)
df$savings_cat_f     <- factor(df$savings_cat, levels=1:4,
                                labels=c("<1 thang","1-3 thang","3-6 thang",">6 thang"))

# ================================================================
# PHAN 1: KIEM DINH GIA DINH
# (Muc 4.1 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 1: KIEM DINH PHAN PHOI CHUAN & PHUONG SAI DONG NHAT\n")
cat("(Muc 4.1 trong bai viet)\n")
cat("================================================================\n")

# 1.1 Normality — 3 tests (Bang 2)
cat("\n--- 1.1 NORMALITY: hhsize (N=3791) --- Bang 2 ---\n")
sw <- shapiro.test(df$hhsize)
ll <- lillie.test(df$hhsize)
ad <- ad.test(df$hhsize)
cat(sprintf("Shapiro-Wilk:     W=%.4f | p=%.2e | %s\n", sw$statistic, sw$p.value, ifelse(sw$p.value<0.05,"FAIL","PASS")))
cat(sprintf("Lilliefors (KS):  D=%.4f | p=%.2e | %s\n", ll$statistic, ll$p.value, ifelse(ll$p.value<0.05,"FAIL","PASS")))
cat(sprintf("Anderson-Darling: A=%.4f | p=%.2e | %s\n", ad$statistic, ad$p.value, ifelse(ad$p.value<0.05,"FAIL","PASS")))
cat("-> Ket luan: Khong dat phan phoi chuan. Theo huong dan GV: dung Kruskal-Wallis.\n")

# 1.2 Homogeneity — 3 tests (Bang 3)
cat("\n--- 1.2 HOMOGENEITY: hhsize ~ income_shock --- Bang 3 ---\n")
lev1 <- leveneTest(hhsize ~ income_shock_f, data=df)
bar1 <- bartlett.test(hhsize ~ income_shock_f, data=df)
fl1  <- fligner.test(hhsize ~ income_shock_f, data=df)
cat(sprintf("Levene:          F=%.4f  | p=%.4f | %s\n", lev1$`F value`[1], lev1$`Pr(>F)`[1], ifelse(lev1$`Pr(>F)`[1]<0.05,"FAIL","PASS")))
cat(sprintf("Bartlett:        K2=%.4f | p=%.4f | %s\n", bar1$statistic, bar1$p.value, ifelse(bar1$p.value<0.05,"FAIL","PASS")))
cat(sprintf("Fligner-Killeen: X2=%.4f | p=%.4f | %s\n", fl1$statistic,  fl1$p.value,  ifelse(fl1$p.value<0.05,"FAIL","PASS")))
cat("-> Ket luan: Ca 3 test PASS — phuong sai dong nhat giua 2 nhom.\n")

# Visualization Phan 1
par(mfrow=c(1,2))
hist(df$hhsize, freq=FALSE, breaks=11, col="steelblue", border="white",
     main="Phan phoi hhsize", xlab="So thanh vien ho")
curve(dnorm(x, mean(df$hhsize), sd(df$hhsize)), add=TRUE, col="red", lwd=2)
legend("topright","Normal LT", col="red", lwd=2, bty="n", cex=0.8)
qqnorm(df$hhsize, main="Q-Q Plot: hhsize", pch=16, col=rgb(0,0,1,0.25))
qqline(df$hhsize, col="red", lwd=2)
par(mfrow=c(1,1))

# ================================================================
# PHAN 2: PHAN TICH DON BIEN
# (Muc 4.2 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 2: PHAN TICH DON BIEN\n")
cat("(Muc 4.2 trong bai viet)\n")
cat("================================================================\n")

# 2.1 Binary variables — Bang 4
cat("\n--- 2.1 TY LE BIEN BINARY (N=3791) --- Bang 4 ---\n")
bin_names  <- c("cut_consumption","income_shock","low_resilience",
                "has_child","employed","male","renter","covid_recent","ethnic_kinh")
bin_labels <- c("Cat giam tieu dung","Cu soc thu nhap","Tiet kiem thap",
                "Co con","Co viec lam","Chu ho nam","Thue nha",
                "Nhiem COVID gan day","Dan toc Kinh")
for(i in seq_along(bin_names)) {
  v <- df[[bin_names[i]]]
  cat(sprintf("%-25s | =1: %4d (%5.1f%%) | =0: %4d (%5.1f%%)\n",
      bin_labels[i], sum(v), mean(v)*100, sum(1-v), (1-mean(v))*100))
}

# 2.2 hhsize descriptive — Bang 5
cat("\n--- 2.2 THONG KE MO TA: hhsize --- Bang 5 ---\n")
cat(sprintf("N=%d | Mean=%.3f | Median=%.0f | SD=%.3f | Min=%d | Max=%d\n",
    length(df$hhsize), mean(df$hhsize), median(df$hhsize),
    sd(df$hhsize), min(df$hhsize), max(df$hhsize)))
cat(sprintf("Q1=%.0f | Q3=%.0f | IQR=%.0f | Skewness=%.3f\n",
    quantile(df$hhsize,.25), quantile(df$hhsize,.75), IQR(df$hhsize),
    mean((df$hhsize-mean(df$hhsize))^3)/sd(df$hhsize)^3))
cat("Phan phoi tan so hhsize:\n")
print(table(df$hhsize))

# 2.3 savings_cat — Bang 6
cat("\n--- 2.3 PHAN PHOI TIET KIEM DU PHONG --- Bang 6 ---\n")
sav_tbl <- as.data.frame(table(df$savings_cat_f))
sav_tbl$Pct <- round(sav_tbl$Freq/nrow(df)*100, 1)
names(sav_tbl) <- c("Muc tiet kiem","N","%")
print(sav_tbl)

# 2.4 region + cut rate — Bang 7
cat("\n--- 2.4 PHAN PHOI THEO VUNG DIA LY & TY LE CAT GIAM --- Bang 7 ---\n")
reg_tbl <- df %>%
  group_by(region_f) %>%
  summarise(N = n(),
            Pct_mau = round(n()/nrow(df)*100, 1),
            N_cat   = sum(cut_consumption),
            TyLe_cat = round(mean(cut_consumption)*100, 1),
            .groups="drop") %>%
  arrange(desc(TyLe_cat))
print(reg_tbl)

# Plots Phan 2
cut_tbl <- data.frame(
  Group = c("Khong cat giam","Co cat giam"),
  N   = c(sum(df$cut_consumption==0), sum(df$cut_consumption==1)),
  Pct = c(mean(df$cut_consumption==0)*100, mean(df$cut_consumption==1)*100)
)
print(ggplot(cut_tbl, aes(x=Group, y=N, fill=Group)) +
  geom_col(width=0.5) +
  geom_text(aes(label=paste0(N,"\n(",round(Pct,1),"%)")), vjust=-0.3, size=4) +
  scale_fill_manual(values=my_cols) +
  labs(title="Phan bo bien phu thuoc: cut_consumption", x="", y="So quan sat") +
  theme(legend.position="none") + ylim(0, max(cut_tbl$N)*1.2))

skew_val <- round(mean((df$hhsize-mean(df$hhsize))^3)/sd(df$hhsize)^3, 3)
print(ggplot(df, aes(x=hhsize)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=1,
                 fill="steelblue", color="white", alpha=0.85) +
  stat_function(fun=dnorm, args=list(mean=mean(df$hhsize), sd=sd(df$hhsize)),
                color="red", linewidth=1.2) +
  scale_x_continuous(breaks=1:12) +
  labs(title="Phan phoi Quy mo Ho gia dinh (hhsize)",
       subtitle=paste0("Mean=",round(mean(df$hhsize),2),
                       " | SD=",round(sd(df$hhsize),2),
                       " | Skewness=",skew_val),
       x="So thanh vien ho", y="Mat do"))

print(ggplot(df, aes(x=savings_cat_f, fill=savings_cat_f)) +
  geom_bar(width=0.6) +
  geom_text(stat="count",
            aes(label=paste0(after_stat(count),
                             " (",round(after_stat(count)/nrow(df)*100,1),"%)")),
            vjust=-0.3, size=3.5) +
  scale_fill_brewer(palette="Blues") +
  labs(title="Phan phoi Tiet kiem du phong (savings_cat)",
       x="Kha nang duy tri chi tieu", y="So ho") +
  theme(legend.position="none") + ylim(0, 1500))

# ================================================================
# PHAN 2.5: THONG KE SUY DIEN
# (Muc 4.3 trong bai viet)
# A. Uoc luong KTC 95%  -> Bang 8
# B. Kiem dinh 1 mau    -> Bang 9
# C. Kiem dinh 2 mau    -> Bang 10
# ================================================================
cat("\n================================================================\n")
cat("PHAN 2.5: THONG KE SUY DIEN\n")
cat("(Muc 4.3 trong bai viet)\n")
cat("A. UOC LUONG KHOANG TIN CAY -> Bang 8\n")
cat("B. KIEM DINH 1 MAU          -> Bang 9\n")
cat("C. KIEM DINH 2 MAU DOC LAP  -> Bang 10\n")
cat("================================================================\n")

# -------------------------------------------------------------------
# A. UOC LUONG KHOANG TIN CAY 95% — Bang 8
# -------------------------------------------------------------------
cat("\n--- A. UOC LUONG KTC 95% --- Bang 8 ---\n")

n <- nrow(df)

# A.1 CI cho ty le cut_consumption
p_cut  <- mean(df$cut_consumption)
ci_cut <- prop.test(sum(df$cut_consumption), n, conf.level = 0.95)$conf.int
cat(sprintf("Ty le ho cat giam tieu dung (cut_consumption=1):\n"))
cat(sprintf("  p = %.4f (%.1f%%) | KTC 95%% = [%.4f ; %.4f]\n",
    p_cut, p_cut*100, ci_cut[1], ci_cut[2]))

# A.2 CI cho ty le income_shock
p_shock  <- mean(df$income_shock)
ci_shock <- prop.test(sum(df$income_shock), n, conf.level = 0.95)$conf.int
cat(sprintf("\nTy le ho bi cu soc thu nhap (income_shock=1):\n"))
cat(sprintf("  p = %.4f (%.1f%%) | KTC 95%% = [%.4f ; %.4f]\n",
    p_shock, p_shock*100, ci_shock[1], ci_shock[2]))

# A.3 CI cho ty le low_resilience
p_res  <- mean(df$low_resilience)
ci_res <- prop.test(sum(df$low_resilience), n, conf.level = 0.95)$conf.int
cat(sprintf("\nTy le ho tiet kiem thap (low_resilience=1):\n"))
cat(sprintf("  p = %.4f (%.1f%%) | KTC 95%% = [%.4f ; %.4f]\n",
    p_res, p_res*100, ci_res[1], ci_res[2]))

# A.4 CI cho trung binh hhsize (dung t.test du hhsize khong chuan — CLT dam bao N=3791)
ci_hh <- t.test(df$hhsize, conf.level = 0.95)
cat(sprintf("\nTrung binh quy mo ho gia dinh (hhsize):\n"))
cat(sprintf("  Mean = %.4f | KTC 95%% = [%.4f ; %.4f]\n",
    ci_hh$estimate, ci_hh$conf.int[1], ci_hh$conf.int[2]))

cat("\n-> Ket luan A (Bang 8):\n")
cat("   - Tren 50% ho do thi cat giam tieu dung thiet yeu trong dich COVID-19\n")
cat("   - Khoang 27% ho chiu cu soc thu nhap\n")
cat("   - KTC hep phan anh mau du lon (N=3.791), do chinh xac uoc luong cao\n")

# -------------------------------------------------------------------
# B. KIEM DINH 1 MAU — Bang 9
# -------------------------------------------------------------------
cat("\n--- B. KIEM DINH 1 MAU --- Bang 9 ---\n")

# B.1 Kiem dinh ty le 1 mau: H0: p(cut_consumption) = 0.50
# [Ket qua nay duoc trinh bay trong Bang 9 cua bai viet]
cat("\nB.1 Kiem dinh ty le 1 mau: cut_consumption\n")
cat("H0: Ty le ho cat giam tieu dung = 50%\n")
cat("H1: Ty le ho cat giam tieu dung != 50%\n")
cat("Phuong phap: Prop.test (Chi-square 1 mau), 2 phia, alpha = 5%\n\n")

pt1 <- prop.test(sum(df$cut_consumption), n, p = 0.50,
                 alternative = "two.sided", conf.level = 0.95)
cat(sprintf("  X2 = %.4f | df = %d | p-value = %.4f\n",
    pt1$statistic, pt1$parameter, pt1$p.value))
cat(sprintf("  KTC 95%% = [%.4f ; %.4f]\n", pt1$conf.int[1], pt1$conf.int[2]))
cat(sprintf("  Ket luan: %s H0 (alpha=5%%)\n",
    ifelse(pt1$p.value < 0.05, "BAC BO", "KHONG BAC BO")))
cat(sprintf("  -> Ty le cat giam (%.1f%%) khac biet co y nghia thong ke so voi 50%%\n",
    mean(df$cut_consumption)*100))

# B.2 Kiem dinh trung vi 1 mau: H0: median(hhsize) = 4
# [Phan tich bo sung — khong duoc trinh bay trong Bang 9 cua bai viet
#  vi Bang 9 chi bao gom kiem dinh ty le (B.1). Ket qua nay tham khao them.]
cat("\n[B.2 — Phan tich bo sung, khong trong Bang 9]\n")
cat("Kiem dinh trung vi 1 mau: hhsize\n")
cat("H0: Trung vi quy mo ho gia dinh = 4 thanh vien\n")
cat("Phuong phap: Wilcoxon Signed-Rank (phi tham so)\n\n")

wsr <- wilcox.test(df$hhsize, mu = 4, alternative = "two.sided")
cat(sprintf("  V = %.0f | p-value = %.4f\n", wsr$statistic, wsr$p.value))
cat(sprintf("  Trung vi quan sat = %.0f thanh vien\n", median(df$hhsize)))
cat(sprintf("  Ket luan: %s H0 (alpha=5%%)\n",
    ifelse(wsr$p.value < 0.05, "BAC BO", "KHONG BAC BO")))
cat("  Luu y: Wilcoxon kiem dinh pseudo-median (Walsh averages), khac voi median\n")
cat("  thong thuong khi phan phoi lech. Ket qua nay khong mau thuan voi median=4.\n")

cat("\n-> Ket luan B (Bang 9):\n")
cat("   - Ty le cat giam vuot nguong 50% co y nghia thong ke:\n")
cat("     COVID-19 tac dong den da so ho do thi, khong chi nhom nho\n")

# -------------------------------------------------------------------
# C. KIEM DINH 2 MAU DOC LAP — Bang 10
# -------------------------------------------------------------------
cat("\n--- C. KIEM DINH 2 MAU DOC LAP --- Bang 10 ---\n")

# C.1 Kiem dinh 2 ty le: cat_consumption giua nhom income_shock
cat("\nC.1 So sanh ty le cat giam giua 2 nhom income_shock\n")
cat("H0: p(cat|shock=1) = p(cat|shock=0)\n")
cat("H1: p(cat|shock=1) != p(cat|shock=0)\n")
cat("Phuong phap: Two-proportion z-test (prop.test), 2 phia, alpha = 5%\n\n")

n_shock1 <- sum(df$income_shock == 1)
n_shock0 <- sum(df$income_shock == 0)
cut_s1   <- sum(df$cut_consumption[df$income_shock == 1])
cut_s0   <- sum(df$cut_consumption[df$income_shock == 0])

p2 <- prop.test(c(cut_s1, cut_s0), c(n_shock1, n_shock0),
                alternative = "two.sided", conf.level = 0.95)
cat(sprintf("  Nhom co cu soc    (n=%4d): %.1f%% cat giam\n", n_shock1, cut_s1/n_shock1*100))
cat(sprintf("  Nhom khong cu soc (n=%4d): %.1f%% cat giam\n", n_shock0, cut_s0/n_shock0*100))
cat(sprintf("  X2 = %.4f | df = %d | p-value = %.2e\n",
    p2$statistic, p2$parameter, p2$p.value))
cat(sprintf("  KTC 95%% chenh lech ty le = [%.4f ; %.4f]\n",
    p2$conf.int[1], p2$conf.int[2]))
cat(sprintf("  Ket luan: %s H0 (alpha=5%%)\n",
    ifelse(p2$p.value < 0.05, "BAC BO", "KHONG BAC BO")))

# C.2 Kiem dinh 2 ty le: cat_consumption giua nhom low_resilience
cat("\nC.2 So sanh ty le cat giam giua 2 nhom low_resilience\n")
cat("H0: p(cat|res=1) = p(cat|res=0)\n")
cat("H1: p(cat|res=1) != p(cat|res=0)\n")
cat("Phuong phap: Two-proportion z-test (prop.test), 2 phia, alpha = 5%\n\n")

n_res1 <- sum(df$low_resilience == 1)
n_res0 <- sum(df$low_resilience == 0)
cut_r1 <- sum(df$cut_consumption[df$low_resilience == 1])
cut_r0 <- sum(df$cut_consumption[df$low_resilience == 0])

p3 <- prop.test(c(cut_r1, cut_r0), c(n_res1, n_res0),
                alternative = "two.sided", conf.level = 0.95)
cat(sprintf("  Nhom tiet kiem thap (n=%4d): %.1f%% cat giam\n", n_res1, cut_r1/n_res1*100))
cat(sprintf("  Nhom tiet kiem du   (n=%4d): %.1f%% cat giam\n", n_res0, cut_r0/n_res0*100))
cat(sprintf("  X2 = %.4f | df = %d | p-value = %.2e\n",
    p3$statistic, p3$parameter, p3$p.value))
cat(sprintf("  KTC 95%% chenh lech ty le = [%.4f ; %.4f]\n",
    p3$conf.int[1], p3$conf.int[2]))
cat(sprintf("  Ket luan: %s H0 (alpha=5%%)\n",
    ifelse(p3$p.value < 0.05, "BAC BO", "KHONG BAC BO")))

# C.3 Mann-Whitney 2 mau: hhsize ~ income_shock
cat("\nC.3 Mann-Whitney 2 mau doc lap: hhsize ~ income_shock\n")
cat("H0: Phan phoi hhsize nhu nhau giua nhom co va khong co cu soc\n")
cat("H1: Phan phoi hhsize khac nhau giua 2 nhom\n")
cat("Phuong phap: Wilcoxon rank-sum (phi tham so), 2 phia, alpha = 5%\n\n")

mw_main <- wilcox.test(hhsize ~ income_shock_f, data = df)
med_s0  <- median(df$hhsize[df$income_shock == 0])
med_s1  <- median(df$hhsize[df$income_shock == 1])
cat(sprintf("  W = %.0f | p-value = %.4f\n", mw_main$statistic, mw_main$p.value))
cat(sprintf("  Trung vi: Khong cu soc = %.0f | Co cu soc = %.0f thanh vien\n", med_s0, med_s1))
cat(sprintf("  Ket luan: %s H0 (alpha=5%%)\n",
    ifelse(mw_main$p.value < 0.05, "BAC BO", "KHONG BAC BO")))

cat("\n-> Ket luan C (Bang 10): Ca 3 kiem dinh 2 mau cho thay su khac biet\n")
cat("   co y nghia thong ke giua cac nhom — ung ho gia thuyet nghien cuu\n")

cat("\n================================================================\n")
cat("KET THUC PHAN 2.5: THONG KE SUY DIEN (Muc 4.3)\n")
cat("================================================================\n")

# ================================================================
# PHAN 3: PHAN TICH HAI BIEN
# (Muc 4.4 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 3: PHAN TICH HAI BIEN\n")
cat("(Muc 4.4 trong bai viet)\n")
cat("================================================================\n")

# 3.1 Chi-square — Bang 11
cat("\n--- 3.1 CHI-SQUARE TESTS: cut_consumption x cac bien --- Bang 11 ---\n")
cat(sprintf("%-22s | chi2     | df | p-value    | CramerV | Sig\n","Bien"))
cat(rep("-",70),"\n",sep="")

chisq_pairs <- list(
  list("male",          "Chu ho nam"),
  list("income_shock",  "Cu soc thu nhap"),
  list("has_child",     "Co con"),
  list("low_resilience","Tiet kiem thap"),
  list("ethnic_kinh",   "Dan toc Kinh"),
  list("renter",        "Thue nha"),
  list("covid_recent",  "COVID gan day"),
  list("employed",      "Co viec lam")
)
chisq_results <- list()
for(pair in chisq_pairs) {
  tbl  <- table(df[["cut_consumption"]], df[[pair[[1]]]])
  test <- chisq.test(tbl)
  v    <- CramerV(tbl)
  sig  <- ifelse(test$p.value<0.001,"***",ifelse(test$p.value<0.01,"**",
           ifelse(test$p.value<0.05,"*","ns")))
  cat(sprintf("%-22s | %8.4f | %2d | %.2e | %.4f  | %s\n",
      pair[[2]], test$statistic, test$parameter, test$p.value, v, sig))
  chisq_results[[pair[[1]]]] <- test
}
h_region <- chisq.test(table(df$region, df$cut_consumption))
cat(sprintf("%-22s | %8.4f | %2d | %.2e | %.4f  | ***\n",
    "Vung dia ly", h_region$statistic, h_region$parameter, h_region$p.value,
    CramerV(table(df$region, df$cut_consumption))))
cat("*** p<0.001 | ** p<0.01 | * p<0.05 | V: cuong do lien he\n")
cat("Xep hang Cramer V (chi bien nhi phan, df=1):\n")
cat("  male(1) > income_shock(2) > has_child(3) > low_resilience(4) > ...\n")
cat("  Vung dia ly (df=5) khong xep hang chung voi bien nhi phan.\n")

# 3.2 Cross-tabulation — Bang 12 & Bang 13
cat("\n--- 3.2 CROSS-TAB: cut_consumption x income_shock --- Bang 12 ---\n")
ct_main <- table(df$income_shock_f, df$cut_consumption_f)
print(ct_main)
cat("\nTy le hang (%):\n")
print(round(prop.table(ct_main, margin=1)*100, 1))

cat("\n--- CROSS-TAB: cut_consumption x low_resilience --- Bang 13 ---\n")
ct2 <- table(df$low_resilience_f, df$cut_consumption_f)
print(ct2)
cat("\nTy le hang (%):\n")
print(round(prop.table(ct2, margin=1)*100, 1))

# 3.3 Mann-Whitney — Bang 14
cat("\n--- 3.3 MANN-WHITNEY TEST: hhsize theo nhom --- Bang 14 ---\n")
cat(sprintf("%-22s | W        | p-value    | Med(0) | Med(1) | Sig\n","Nhom"))
cat(rep("-",68),"\n",sep="")
mw_groups <- list(
  list("income_shock",    "Cu soc thu nhap"),
  list("cut_consumption", "Cat giam TD"),
  list("low_resilience",  "Tiet kiem thap"),
  list("has_child",       "Co con"),
  list("employed",        "Co viec lam")
)
for(g in mw_groups) {
  g0   <- df$hhsize[df[[g[[1]]]]==0]
  g1   <- df$hhsize[df[[g[[1]]]]==1]
  test <- wilcox.test(g0, g1)
  sig  <- ifelse(test$p.value<0.001,"***",ifelse(test$p.value<0.01,"**",
           ifelse(test$p.value<0.05,"*","ns")))
  cat(sprintf("%-22s | %8.0f | %.2e | %6.1f | %6.1f | %s\n",
      g[[2]], test$statistic, test$p.value, median(g0), median(g1), sig))
}

# 3.4 Spearman correlation — Bang 15
cat("\n--- 3.4 MA TRAN TUONG QUAN SPEARMAN --- Bang 15 ---\n")
cor_vars <- c("cut_consumption","income_shock","low_resilience",
              "hhsize","has_child","employed","renter","covid_recent","male")
cor_mat  <- cor(df[,cor_vars], method="spearman", use="complete.obs")

cat("\nToan bo ma tran Spearman:\n")
print(round(cor_mat, 3))

# Cac cap chon loc cho Bang 15 trong bai
cat("\n--- CAC CAP SPEARMAN CHON LOC (Bang 15) ---\n")
selected_pairs <- list(
  c("cut_consumption","male"),
  c("cut_consumption","income_shock"),
  c("cut_consumption","has_child"),
  c("cut_consumption","low_resilience"),
  c("hhsize","has_child"),
  c("income_shock","covid_recent")
)
cat(sprintf("%-40s | rho\n","Cap bien"))
cat(rep("-",50),"\n",sep="")
for(p in selected_pairs) {
  rho <- cor_mat[p[1], p[2]]
  cat(sprintf("%-40s | %+.3f\n", paste(p[1], "--", p[2]), rho))
}

corrplot(cor_mat, method="color", type="upper",
         addCoef.col="black", number.cex=0.65,
         tl.col="black", tl.srt=45, tl.cex=0.8,
         col=colorRampPalette(c("#F44336","white","#2196F3"))(200),
         title="Ma tran Tuong quan Spearman", mar=c(0,0,2,0))

# Plots Phan 3
ct_prop <- as.data.frame(prop.table(table(df$income_shock_f, df$cut_consumption_f), 1))
names(ct_prop) <- c("income_shock","cut_consumption","Prop")
print(ggplot(ct_prop[ct_prop$cut_consumption=="Co cat",],
       aes(x=income_shock, y=Prop*100, fill=income_shock)) +
  geom_col(width=0.5) +
  geom_text(aes(label=paste0(round(Prop*100,1),"%")), vjust=-0.4, size=4.5) +
  scale_fill_manual(values=my_cols) +
  labs(title="Ty le cat giam tieu dung theo Cu soc thu nhap",
       x="", y="% Ho co cat giam") +
  theme(legend.position="none") + ylim(0, 80))

reg_rate <- df %>%
  group_by(region_f) %>%
  summarise(Rate=mean(cut_consumption)*100, N=n(), .groups="drop")
print(ggplot(reg_rate, aes(x=reorder(region_f, Rate), y=Rate, fill=Rate)) +
  geom_col(width=0.6) +
  geom_text(aes(label=paste0(round(Rate,1),"%")), hjust=-0.1, size=3.5) +
  coord_flip() +
  scale_fill_gradient(low="#90CAF9", high="#B71C1C") +
  labs(title="Ty le cat giam tieu dung theo Vung dia ly",
       x="", y="% Ho cat giam") +
  theme(legend.position="none") + ylim(0, 75))

# ================================================================
# PHAN 4: KRUSKAL-WALLIS (Thay ANOVA)
# (Muc 4.5 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 4: KRUSKAL-WALLIS TEST (Phi tham so - thay ANOVA)\n")
cat("(Muc 4.5 trong bai viet)\n")
cat("Ly do: hhsize khong dat phan phoi chuan (ca 3 test deu FAIL)\n")
cat("Theo huong dan giang vien\n")
cat("================================================================\n")

kw1 <- kruskal.test(hhsize ~ income_shock_f,    data=df)
kw2 <- kruskal.test(hhsize ~ cut_consumption_f, data=df)
kw3 <- kruskal.test(hhsize ~ region_f,          data=df)
kw4 <- kruskal.test(hhsize ~ has_child_f,       data=df)
kw5 <- kruskal.test(hhsize ~ employed_f,        data=df)
kw6 <- kruskal.test(hhsize ~ low_resilience_f,  data=df)

# Bang 16: sap xep theo chi2 giam dan (khop voi bai viet)
cat("\n--- BANG TONG KET KRUSKAL-WALLIS (sap xep chi2 giam dan) --- Bang 16 ---\n")
cat(sprintf("%-30s | chi2      | df | p-value    | Ket luan\n","Phan nhom"))
cat(rep("-",72),"\n",sep="")
kw_sorted <- list(
  list(kw4,"hhsize ~ has_child"),
  list(kw5,"hhsize ~ employed"),
  list(kw1,"hhsize ~ income_shock"),
  list(kw2,"hhsize ~ cut_consumption"),
  list(kw6,"hhsize ~ low_resilience"),
  list(kw3,"hhsize ~ region")
)
for(kw in kw_sorted) {
  k <- kw[[1]]; nm <- kw[[2]]
  cat(sprintf("%-30s | %9.4f | %2d | %.2e | %s\n",
      nm, k$statistic, k$parameter, k$p.value,
      ifelse(k$p.value<0.001,"Bac bo H0 ***",
      ifelse(k$p.value<0.05, "Bac bo H0 *","Khong bac bo H0"))))
}
cat("H0: Phan phoi hhsize nhu nhau giua cac nhom\n")

cat("\n--- POST-HOC DUNN TEST: hhsize ~ region (Bonferroni) ---\n")
dunn.test(df$hhsize, df$region_f, method="bonferroni", kw=TRUE, label=TRUE)

cat("\n--- MEDIAN hhsize THEO NHOM income_shock ---\n")
df %>% group_by(income_shock_f) %>%
  summarise(N=n(), Median=median(hhsize), Mean=round(mean(hhsize),2), .groups="drop") %>%
  print()

print(ggplot(df, aes(x=has_child_f, y=hhsize, fill=has_child_f)) +
  geom_boxplot(alpha=0.75) +
  stat_summary(fun=median, geom="point", shape=18, size=4, color="black") +
  scale_fill_manual(values=c("#4CAF50","#FF9800")) +
  labs(title="Kruskal-Wallis: hhsize ~ has_child",
       subtitle=sprintf("X2=%.3f, df=%d, p=%.2e", kw4$statistic, kw4$parameter, kw4$p.value),
       x="", y="So thanh vien ho") +
  theme(legend.position="none"))

print(ggplot(df, aes(x=income_shock_f, y=hhsize, fill=income_shock_f)) +
  geom_boxplot(alpha=0.75, outlier.color="red", outlier.alpha=0.3) +
  stat_summary(fun=median, geom="point", shape=18, size=4, color="black") +
  scale_fill_manual(values=my_cols) +
  labs(title="Kruskal-Wallis: hhsize ~ income_shock",
       subtitle=sprintf("X2=%.3f, df=%d, p=%.4f", kw1$statistic, kw1$parameter, kw1$p.value),
       x="", y="So thanh vien ho") +
  theme(legend.position="none"))

print(ggplot(df, aes(x=region_f, y=hhsize, fill=region_f)) +
  geom_boxplot(alpha=0.75) +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black") +
  labs(title="Kruskal-Wallis: hhsize ~ region",
       subtitle=sprintf("X2=%.3f, df=%d, p=%.3f", kw3$statistic, kw3$parameter, kw3$p.value),
       x="", y="So thanh vien ho") +
  theme(axis.text.x=element_text(angle=25, hjust=1), legend.position="none"))

# ================================================================
# PHAN 5: KIEM DINH GIA THUYET
# (Muc 4.7.3 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 5: KIEM DINH GIA THUYET\n")
cat("(Muc 4.7.3 trong bai viet)\n")
cat("================================================================\n")

# Bang 19
cat("\n--- Bang 19: TONG HOP 7 GIA THUYET ---\n")
hyp <- list(
  list("income_shock",   "H1: Cu soc thu nhap -> cat giam TD"),
  list("low_resilience", "H2: Tiet kiem thap  -> cat giam TD"),
  list("has_child",      "H3: Co con           -> cat giam TD"),
  list("employed",       "H4: Co viec lam      -> cat giam TD"),
  list("covid_recent",   "H5: COVID gan day    -> cat giam TD")
)
for(h in hyp) {
  tbl  <- table(df[["cut_consumption"]], df[[h[[1]]]])
  test <- chisq.test(tbl)
  v    <- CramerV(tbl)
  cat(sprintf("\n%s\n  X2=%.4f, df=%d, p=%.2e | V=%.4f | %s H0\n",
      h[[2]], test$statistic, test$parameter, test$p.value, v,
      ifelse(test$p.value<0.05,"BAC BO","KHONG BAC BO")))
}
cat(sprintf("\nH6: hhsize khac nhau giua nhom income_shock\n  X2=%.4f, df=%d, p=%.4f | %s H0\n",
    kw1$statistic, kw1$parameter, kw1$p.value,
    ifelse(kw1$p.value<0.05,"BAC BO","KHONG BAC BO")))
cat(sprintf("\nH7: Ty le cat giam TD khac nhau giua cac vung\n  X2=%.4f, df=%d, p=%.2e | %s H0\n",
    h_region$statistic, h_region$parameter, h_region$p.value,
    ifelse(h_region$p.value<0.05,"BAC BO","KHONG BAC BO")))

# ================================================================
# PHAN 6: KIEM TRA GIA DINH MO HINH HOI QUY — VIF
# (Muc 4.6 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 6: KIEM TRA GIA DINH MO HINH HOI QUY — VIF\n")
cat("(Muc 4.6 trong bai viet)\n")
cat("================================================================\n")

cat("\n--- 6.1 KIEM DINH DA CONG TUYEN (VIF) --- Bang 17 ---\n")
cat("Luu y: VIF tinh tren mo hinh xac suat tuyen tinh (LPM) tuong duong\n")
cat("vi ham vif() khong ap dung truc tiep cho Logistic Regression.\n\n")

lpm_model <- lm(cut_consumption ~ income_shock + low_resilience + hhsize +
                  has_child + employed + male + ethnic_kinh + renter +
                  covid_recent + reg_red_river + reg_midlands +
                  reg_central_coast + reg_cent_highlands + reg_southeast,
                data=df)

vif_values <- vif(lpm_model)

cat(sprintf("%-22s | %8s | %s\n", "Bien", "VIF", "Danh gia"))
cat(rep("-", 52), "\n", sep="")
for(i in seq_along(vif_values)) {
  cat(sprintf("%-22s | %8.4f | %s\n",
      names(vif_values)[i], vif_values[i],
      ifelse(vif_values[i] < 5, "Binh thuong",
      ifelse(vif_values[i] < 10, "Canh bao", "Nghiem trong"))))
}
cat(rep("-", 52), "\n", sep="")
cat(sprintf("VIF toi da:     %.4f (bien: %s)\n",
    max(vif_values), names(vif_values)[which.max(vif_values)]))
cat(sprintf("VIF trung binh: %.4f\n", mean(vif_values)))
cat("\nNguong canh bao: VIF > 5 | Nguong nghiem trong: VIF > 10\n")
cat("-> Ket luan: Tat ca VIF < 2.1, khong co da cong tuyen nghiem trong.\n")

# ================================================================
# PHAN 7: HOI QUY LOGISTIC
# (Muc 4.7 trong bai viet)
# ================================================================
cat("\n================================================================\n")
cat("PHAN 7: HOI QUY LOGISTIC | DV: cut_consumption\n")
cat("(Muc 4.7 trong bai viet)\n")
cat("================================================================\n")

model1 <- glm(cut_consumption ~ income_shock + low_resilience + hhsize +
                has_child + employed + male + ethnic_kinh + renter +
                covid_recent + reg_red_river + reg_midlands +
                reg_central_coast + reg_cent_highlands + reg_southeast,
              data=df, family=binomial(link="logit"))

cat("\n--- 7.1 KET QUA MO HINH CO SO (summary) ---\n")
print(summary(model1))

# Bang 18: OR + CI (Wald) + p
cat("\n--- 7.2 ODDS RATIOS (OR = exp(Beta)) voi KTC 95% Wald --- Bang 18 ---\n")
beta <- coef(model1)
ci   <- confint.default(model1)   # Wald CI — nhat quan voi bai viet
pval <- summary(model1)$coefficients[,4]
or_df <- data.frame(
  Bien    = names(beta),
  Beta    = round(beta, 4),
  OR      = round(exp(beta), 4),
  CI_low  = round(exp(ci[,1]), 4),
  CI_high = round(exp(ci[,2]), 4),
  p       = round(pval, 4),
  Sig     = ifelse(pval<0.001,"***",ifelse(pval<0.01,"**",
             ifelse(pval<0.05,"*","ns")))
)
print(or_df)
cat("OR > 1: tang so chen cat giam | OR < 1: giam so chen | *** p<0.001\n")
cat("Luu y: OR do luong SO CHEN (odds), KHONG phai xac suat truc tiep.\n")

# Bang 20: do phu hop
cat("\n--- 7.3 DO PHU HOP MO HINH --- Bang 20 ---\n")
null_m  <- glm(cut_consumption~1, data=df, family=binomial)
mcf_r2  <- as.numeric(1 - logLik(model1)/logLik(null_m))
hl_test <- hoslem.test(df$cut_consumption, fitted(model1), g=10)
roc_obj <- roc(df$cut_consumption, fitted(model1), quiet=TRUE)
cat(sprintf("AIC:             %.2f\n",  AIC(model1)))
cat(sprintf("Log-Likelihood:  %.2f\n",  as.numeric(logLik(model1))))
cat(sprintf("McFadden R2:     %.4f\n",  mcf_r2))
cat(sprintf("Hosmer-Lemeshow: X2=%.4f, p=%.4f -> %s\n",
    hl_test$statistic, hl_test$p.value,
    ifelse(hl_test$p.value>0.05,"Mo hinh phu hop","Kiem tra lai")))
cat(sprintf("AUC:             %.4f\n", auc(roc_obj)))

# Bang 21: mo hinh tuong tac
cat("\n--- 7.4 MO HINH MO RONG: Interaction income_shock x low_resilience --- Bang 21 ---\n")
model2 <- glm(cut_consumption ~ income_shock * low_resilience + hhsize +
                has_child + employed + male + ethnic_kinh + renter +
                covid_recent + reg_red_river + reg_midlands +
                reg_central_coast + reg_cent_highlands + reg_southeast,
              data=df, family=binomial(link="logit"))
int_coef <- coef(summary(model2))["income_shock:low_resilience",]
cat(sprintf("Interaction: Beta=%.4f | OR=%.4f | p=%.4f | %s\n",
    int_coef[1], exp(int_coef[1]), int_coef[4],
    ifelse(int_coef[4]<0.05,"Co y nghia *","Khong co y nghia ns")))
cat(sprintf("AIC Model 1: %.2f | AIC Model 2: %.2f | Delta AIC=%.2f -> %s\n",
    AIC(model1), AIC(model2), AIC(model2)-AIC(model1),
    ifelse(AIC(model2)<AIC(model1),"Model 2 tot hon","Model 1 duoc giu lai")))

cat("\n--- LRT: Model 1 vs Model 2 ---\n")
print(anova(model1, model2, test="LRT"))

# OR Forest plot
or_plot <- or_df[or_df$Bien!="(Intercept)",]
or_plot$Bien <- factor(or_plot$Bien, levels=or_plot$Bien[order(or_plot$OR)])
print(ggplot(or_plot, aes(x=Bien, y=OR, ymin=CI_low, ymax=CI_high, color=OR>1)) +
  geom_hline(yintercept=1, linetype="dashed", color="gray50", linewidth=0.8) +
  geom_errorbar(width=0.3, linewidth=0.8) +
  geom_point(size=3) +
  coord_flip() +
  scale_color_manual(values=c("#2196F3","#F44336"),
                     labels=c("Giam so chen cat giam","Tang so chen cat giam")) +
  labs(title="Odds Ratios — Logistic Regression (Model 1)",
       subtitle="Diem = OR | Duong = KTC 95% Wald | Dut = OR=1",
       x="", y="Odds Ratio (So chen)", color="") +
  theme(legend.position="bottom"))

# ROC Curve
plot(roc_obj, main=sprintf("ROC Curve | AUC = %.4f", auc(roc_obj)),
     col="#2196F3", lwd=2)
abline(a=0, b=1, lty=2, col="gray60")

# Predicted probability distribution
df$pred_prob <- fitted(model1)
print(ggplot(df, aes(x=pred_prob, fill=cut_consumption_f)) +
  geom_histogram(alpha=0.7, bins=30, position="identity") +
  scale_fill_manual(values=my_cols) +
  labs(title="Phan phoi xac suat du bao tu mo hinh Logistic",
       x="P(cut_consumption=1)", y="So quan sat", fill="") +
  theme(legend.position="bottom"))

cat("\n================================================================\n")
cat("HOAN TAT TOAN BO PHAN TICH — Nhom 10 | 26D1MAT50801003\n")
cat("================================================================\n")
