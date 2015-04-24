#dependendy
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
#devtools::install_github("petermeissner/wikipediatrend")

library(AnomalyDetection)
##################

createDay <- function(noise=0) {
  point=seq(0, pi, by=0.02)
  connection=sin(point)
  noise <- rnorm(length(point), 0, noise)
  return(connection+noise)
}

createDays <- function(totalDays, noise=0) {
  allDays <- c()
  for (day in 1:totalDays ) {
    allDays <- c(allDays, createDay(noise))
  }
  return(allDays)
}

createStrangeDay <- function(noise=0, strangePos=20, strangeN=9, strangeMean=0.45, strangeSd=0.02) {
  normalDay <- createDay(noise)
  pointPerDay <- length(normalDay)
  
  beginNormalDay <- normalDay[0:strangePos]
  strange <- rnorm(strangeN, mean=strangeMean, sd=strangeSd)
  endNormalDay <- normalDay[(strangePos+strangeN+1):pointPerDay]
  
  strangeDay <- append(append(beginNormalDay, strange), endNormalDay)
  return(strangeDay)
}

############# detection #######
detectAnomaly <- function(usual = createDays(10, 0.05), strange = NULL){
  all <- c(usual, strange)
  plot(as.ts(all))
  
  res = AnomalyDetectionVec(all, max_anoms=0.015, period=pointPerDay, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}

# Too much on the morning
bumpToEarly <- function(){
  set.seed(4321)
  strange <- createStrangeDay(noise = 0.05, strangePos = 25, strangeN = 10, strangeMean = 0.85)
  detectAnomaly(strange = strange)
}

# Always the same. Introduct a very small change on a strict model
smallChangeOnStrictModel <- function(){
  set.seed(4321)
  usual <- createDays(10, 0)
  strange <- createDay(0)
  strange[40] = strange[40]-0.001
  detectAnomaly(usual = usual, strange = strange)
}

# small change
smallChange <- function(){
  set.seed(4321)
  usual <- createDays(10, 0)
  strange <- createDay(0)
  parcel <- strange[40:60]
  strange[40:50] = parcel+rnorm(n = length(parcel),mean = 0, sd = 0.06)
  detectAnomaly(usual = usual, strange = strange)
}

# More variation in the noise
moreNoise <- function(){
  set.seed(4321)
  usual <- createDays(10, 0.1)
  strange <- createStrangeDay(noise = 0.2, strangePos = 30, strangeN = 9, strangeMean = 0.75)
  detectAnomaly(usual = usual, strange = strange)
}

plateau <- function(){
  set.seed(4321)
  usual <- createDays(10, 0.1)
  strange <- createDay(0.1)
  
  strange[60:120] <- strange[60:120]/2

  detectAnomaly(usual = usual, strange = strange)
}



# there is an unusual grow
growSuddenly <- function(){
  set.seed(4321)
  usual <- createDays(10, 0.1)
  strange <- createDay(noise = 0.1)*1.6
  detectAnomaly(usual = usual, strange = strange)
}

removeNoise <- function(){
  set.seed(4321)
  usual <- createDays(10, 0.1)
  strange <- createDay(noise = 0.0)
  detectAnomaly(usual = usual, strange = strange)
}

removeNoise <- function(){
  set.seed(4321)
  usual <- createDays(10, 0.1)
  strange <- createDay(noise = 0.0)
  detectAnomaly(usual = usual, strange = strange)
}

#it just stop working: zero everywhere
stopSuddenly <- function(){
  set.seed(4321)
  usualWeek <- createDays(10, 0.1)
  usualDay <- createDay(noise = 0.1)
  notFlat <- usualDay[0:60]
  flat <- rep(0, length(usualDay)-length(notFlat))
  strange <- c(notFlat, flat)
  detectAnomaly(usual = usualWeek, strange = strange)
}

flat <- function(){
  set.seed(4321)
  usualWeek <- createDays(10, 50)
  flatDay <- createDay(noise = 0)
  detectAnomaly(usual = usualWeek, strange = flatDay)
}

floor <- function(){
  set.seed(4321)
  pointPerDay <- length(createDay())
  usualWeek <- rnorm(n = pointPerDay*7, mean = 1, sd = 0.05)
  floorDay <- rnorm(n = pointPerDay, mean = 1.5, sd = 0.05)
  detectAnomaly(usual = usualWeek, strange = floorDay)
}

speark <- function(){
  set.seed(4321)
  pointPerDay <- length(createDay())
  usualWeek <- rnorm(n = pointPerDay*7, mean = 1, sd = 0.05)
  floorDay <- rnorm(n = pointPerDay, mean = 1, sd = 0.05)
  floorDay[80] <- 1.5
  detectAnomaly(usual = usualWeek, strange = floorDay)
}

tmp <- function(){
  set.seed(4325)
  blabla <- rnorm(n = 2400, mean = 10, sd = 0.1)
  blabla[2200] <- 12
  
  res = AnomalyDetectionVec(blabla, max_anoms=0.0001, alpha = 0.4, period=400, direction='both', only_last=T, plot=TRUE)
  res$plot
}

bumpInDoublePick <- function(){
  set.seed(4321)
  usual <- createDays(14, 0.05)
  strange <- c(createDay(noise = 0.05), createDay(noise = 0.05))
  from <- length(strange)/2
  strange[(from):(from+5)] <- 0.5
  
  all <- c(usual, strange)
  plot(as.ts(all))
  
  res = AnomalyDetectionVec(all, max_anoms=0.49, period=pointPerDay*2, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}

exponentialGrow <- function(){
  set.seed(4321)
  allDays <- c()
  for (day in 1:14 ) {
    allDays <- c(allDays, createDay(0.2)*(day^2))
  }
  
  plot(as.ts(allDays))
  res = AnomalyDetectionVec(allDays, max_anoms=0.49, period=pointPerDay, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}

linearGrowData <- function(){
  set.seed(4321)
  allDays <- c()
  for (day in 1:14 ) {
    allDays <- c(allDays, createDay(0.2)*day)
  }
  return(allDays)
}

linearGrow <- function(){
  allDays <- linearGrowData()

  plot(as.ts(allDays))
  res = AnomalyDetectionVec(allDays, max_anoms=0.49, period=pointPerDay, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}

linearGrowWithError <- function(){
  allDays <- linearGrowData()
  allDays[length(allDays)-(pointPerDay/2)] <- -3
  plot(as.ts(allDays))
  res = AnomalyDetectionVec(allDays, max_anoms=0.49, period=pointPerDay, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}

justGrow <- function(){
  grow <- seq(1:(pointPerDay*14))*2+10
  plot(as.ts(grow))
  res = AnomalyDetectionVec(grow, max_anoms=0.49, period=pointPerDay, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}

justGrowWithError <- function(){
  grow <- seq(1:(pointPerDay*14))*2+10
  
  end <- length(grow)-200
  grow[end:(end+50)] <- 20
  
  plot(as.ts(grow))
  res = AnomalyDetectionVec(grow, max_anoms=0.49, period=pointPerDay, direction='both', only_last=TRUE, plot=TRUE)
  res$plot
}


pointPerDay <- length(createDay())

#sucess
bumpToEarly()
smallChangeOnStrictModel()
smallChange()
moreNoise()
plateau()
growSuddenly()
floor()
speark()
bumpInDoublePick()
justGrow()
linearGrow()


#fail
removeNoise()
flat()
exponentialGrow()
linearGrowWithError()
justGrowWithError()




######test####
sameNumberOfDay <- length(createDay(1)) == length(createStrangeDay(1)) && length(createDay(1)) ==  length(createStrangeDay2(2))
if (sameNumberOfDay) {
  print("OK")
} else {
  print("KO")
}

sameNumberOfDay <- length(createDays(2)) == (length(createDays(1)) + length(createDays(1)))
if (sameNumberOfDay) {
  print("OK")
} else {
  print("KO")
}

