##BootStrap##
#############

#평균_150000, 분산_50000인 정규분포 난수 발생 
Moneys <- c(round(rnorm(n=20, mean=150000, sd=50000)))

#numeric 10,000,000개 공간 확보
boot <- numeric(10000000)

#루프, 복원 재추출
for(i in 1:length(boot)) boot[i] <- mean(sample(Moneys, replace = T))
#평균
round(mean(boot))
#신뢰구간
LL <- quantile(boot,0.025)
UL <- quantile(boot,0.975)
cat("Empirical 95% Confidence Interval:",LL,UL,"\n")

#전통적인 t.test와 비교
t.test(Moneys)

#그래프에 의한 정규성 검정
par(mfrow=c(1,2))
hist(boot)
hist(Moneys)

#통계량으로 정규성 검정
#귀무가설 H0 : 모집단은 정규분포를 따른다
#대립가설 H1 : 모집단은 정규분포를 따르지 않는다
shapiro.test(Moneys)

#부트스트랩 결과 요약
summary(boot)