library(evd)
require(trend)

calculate_stcs = function(data, me, ts, updateProgress = NULL){
  no = c(); na = c(); min = c(); max = c(); fq = c(); tq = c(); mea = c()
  med = c(); sum = c(); sem = c(); lcl = c(); ucl = c(); var = c(); std = c()
  skn = c(); kts = c()
  
  for(i in seq(0, 12)){
    no = append(no, basicStats(maxs(data, me, ts, i))[[1,1]])
    na = append(na, basicStats(maxs(data, me, ts, i))[[2,1]])
    min = append(min, basicStats(maxs(data, me, ts, i))[[3,1]])
    max = append(max, basicStats(maxs(data, me, ts, i))[[4,1]])
    fq = append(fq, basicStats(maxs(data, me, ts, i))[[5,1]])
    tq = append(tq, basicStats(maxs(data, me, ts, i))[[6,1]])
    mea = append(mea, basicStats(maxs(data, me, ts, i))[[7,1]])
    med = append(med, basicStats(maxs(data, me, ts, i))[[8,1]])
    sum = append(sum, basicStats(maxs(data, me, ts, i))[[9,1]])
    sem = append(sem, basicStats(maxs(data, me, ts, i))[[10,1]])
    lcl = append(lcl, basicStats(maxs(data, me, ts, i))[[11,1]])
    ucl = append(ucl, basicStats(maxs(data, me, ts, i))[[12,1]])
    var = append(var, basicStats(maxs(data, me, ts, i))[[13,1]])
    std = append(std, basicStats(maxs(data, me, ts, i))[[14,1]])
    skn = append(skn, basicStats(maxs(data, me, ts, i))[[15,1]])
    kts = append(kts, basicStats(maxs(data, me, ts, i))[[16,1]])
    
    if (is.function(updateProgress)) {
      text = paste0("Calculando")
      updateProgress(detail = text)
    }
  }
  
  return(list(no, na, min, max, fq, tq, mea, med, sum, sem, lcl, ucl, var, std,
              skn, kts))
}

maxs = function(data, me, ts, mo) {
  newdata = data; maxs = NULL
  
  if(mo == 0){
    attach(data)
  }
  else{
    newdata = subset(data, month == as.character(mo))
    
    attach(newdata)
  }
  
  if(me == 1){ 
    maxs = tapply(var, year, max)

    maxs = maxs[complete.cases(maxs)]
  }
  else{
    newdata = subset(newdata, select = var)
    
    newdata = newdata[complete.cases(newdata),]
    
    maxs = newdata[newdata>ts]
  }
  
  return(maxs)
}

generate_graphs = function(n, data, time, period, name, uof, par, method,
                           updateProgress = NULL) {
  
  var = train_test(maxs(data, method, mo = n), time)[[1]]
  
  if (is.function(updateProgress)) {
    text = paste0("Renderizando")
    updateProgress(detail = text)
  }
  
  df = data.frame(var)
  
  breaks = pretty(range(var), n = nclass.Sturges(var), min.n = 1)
  
  ggplot(data = df, aes(x = var)) + 
    geom_histogram(breaks = breaks, fill="white", 
                   colour = "grey", aes(y = after_stat(density))) +
    geom_line(size = 1.5, aes(y = after_stat(density), colour = 'Empírico'), 
              stat = 'density') +
    stat_function(size = 1.5, fun=dgev, args=list(par[[2]][n+1], par[[3]][n+1], 
                                      par[[4]][n+1]), aes(colour = "GVE")) +
    stat_function(size = 1.5, fun=dgumbel, args=list(par[[6]][n+1], par[[7]][n+1]), 
                  aes(colour = "Gumbel")) +
    labs(title = paste0("Ajustes do modelo GVE considerando o período ", 
                       tolower(period[n+1])),
         x = paste0("Extremos de ", tolower(name), " (", tolower(uof), ")"),
         y = "Densidade") +
    scale_color_manual(values=c("black", "#e74c3c", "#3498db")) +
    labs(color = "") +
    theme_classic(base_size = 16)
}

parameters_block = function(data, time, method, updateProgress = NULL) {
  mi_gev = c(); sigma_gev = c(); csi_gev = c(); mi_gum = c(); sigma_gum = c()
  adj_gev = 0; adj_gum = 0; x = 0; gev = list(); gum = list()
  
  for(i in seq(0, 12)){
    
    adj_gev = fgev(train_test(maxs(data, method, mo = i), time)[[1]])
    gev = append(gev, list(adj_gev))
    adj_gum = fgev(train_test(maxs(data, method, mo = i), time)[[1]], shape = 0)
    gum = append(gum, list(adj_gum))
    
    mi_gev = append(mi_gev, adj_gev$estimate[1])
    sigma_gev = append(sigma_gev, adj_gev$estimate[2])
    csi_gev = append(csi_gev, adj_gev$estimate[3])
    
    mi_gum = append(mi_gum, adj_gum$estimate[1])
    sigma_gum = append(sigma_gum, adj_gum$estimate[2])
    
    if (is.function(updateProgress)) {
      text = paste0("Calculando")
      updateProgress(detail = text)
    }
  }
  
  return(list(gev, mi_gev, sigma_gev, csi_gev, gum, mi_gum, sigma_gum))
}

probs_rls = function(data, time, lvls, rts, sl, par, gof,
                     updateProgress = NULL) {
  dtb = c(); prb = list(); rl = list(); sl = ifelse(is.na(sl), 0.05, sl)
  
  for(i in seq(1, 12)){
    prb = append(prb, list(NULL))
    rl = append(rl, list(NULL))
  }
  
  for(i in seq(1,13)) {
    
    gev = 0; gum = 0
    
    if(unlist(gof[[3]][i]) > sl) 
      gum = gum + 1
    
    if(unlist(gof[[4]][i]) > sl)
      gev = gev + 1
    
    if(unlist(gof[[5]][i]) > sl)
      gum = gum + 1
    
    if(unlist(gof[[6]][i]) > gof[[7]][i])
      gum = gum + 2 
    else
      gev = gev + 2
    
    if(gev > gum){
      dtb = append(dtb, "GVE")
      for(j in seq(1, 12)){
        prb[[j]] = append(prb[[j]], pgev(lvls[j], unlist(par[[2]][[i]]), 
                                         unlist(par[[3]][[i]]), 
                                         unlist(par[[4]][[i]]), 
                                         lower.tail=FALSE)*100)
        
        rl[[j]] = append(rl[[j]], qgev(1-(1/rts[j]), unlist(par[[2]][[i]]), 
                                       unlist(par[[3]][[i]]), 
                                       unlist(par[[4]][[i]])))
      }
    }
    else{
      dtb = append(dtb, "Gumbel")
      for(j in seq(1, 12)){
        prb[[j]] = append(prb[[j]], pgumbel(lvls[j], unlist(par[[6]][[i]]),
                                            unlist(par[[7]][[i]]), 
                                            lower.tail=FALSE)*100)
        
        rl[[j]] = append(rl[[j]], qgumbel(1-(1/rts[j]), unlist(par[[6]][[i]]),
                                          unlist(par[[7]][[i]])))
      }  
    }
    
    if (is.function(updateProgress)) {
      text = paste0("Calculando")
      updateProgress(detail = text)
    }
  }
  return(list(dtb, prb, rl))
}

table_gof = function(data, time, par, method, updateProgress = NULL) {
  mkt = c(); qt = c(); lrt = c(); kst_gev = c(); kst_gum = c(); mape_gev = c()
  mape_gum = c()
  
  for(i in seq(0, 12)){
    
    tempo = time; sqc = c()
    
    train = train_test(maxs(data, method, mo = i), tempo)[[1]]
    test = train_test(maxs(data, method, mo = i), tempo)[[2]]
    tyear = as.numeric(names(test))
    end = as.numeric(names(test[length(test)])) - tempo
    tempo = time + 2
    
    for (u in seq(2, end)) {
      if(tyear[u-1] == tempo)
        sqc = append(sqc, u) 
      else
        sqc = append(sqc, u + (tyear[u-1] - tempo))
      
      if(length(tyear) == u-1)
        break 
      else
        tempo = tempo + 1
    }
    
    mkt = append(mkt, mk.test(train)$p.value)
    qt = append(qt, Box.test(train, type = c("Ljung-Box"))$p.value)
    lrt = append(
      lrt, pchisq((par[[5]][[i+1]]$deviance - par[[1]][[i+1]]$deviance),
                  1, lower.tail = FALSE))
    kst_gev = append(
      kst_gev, 
      ks.test(train, "pgev", par[[2]][i+1], par[[3]][i+1], 
              par[[4]][i+1])$p.value)
    kst_gum = append(
      kst_gum,
      ks.test(train, "pgumbel", par[[6]][i+1], par[[7]][i+1])$p.value)
    
    rl_gev = qgev(1-(1/sqc), par[[2]][i+1], par[[3]][i+1], par[[4]][i+1])
    mape_gev = append(
      mape_gev,
      (1/length(sqc))*sum(abs((rl_gev-test)/test))*100)
    
    rl_gum = qgumbel(1-(1/sqc), par[[6]][i+1], par[[7]][i+1])
    mape_gum = append(
      mape_gum,
      (1/length(sqc))*sum(abs((rl_gum-test)/test))*100)
    
    if (is.function(updateProgress)) {
      text = paste0("Calculando")
      updateProgress(detail = text)
    }
  }
  
  return(list(mkt, qt, lrt, kst_gev, kst_gum, mape_gev, mape_gum))
}

train_test = function(data, time) {
  train = data[names(data) <= time]
  test = data[names(data) > time & names(data) <= max(as.numeric(names(data)))] 
  test = test[-1] 
  
  return(list(train, test))
}

treat_data = function(data) {
  data = subset(data, select = var)
  
  data = data[complete.cases(data),]
  
  return(data)
}

treat_file = function(path) {
  data = read.table(path, header=TRUE, dec=",")
  attach(data)
  
  dvd_data = data.frame(month, year, var)
  
  dvd_data = dvd_data[complete.cases(dvd_data),]
  
  detach(data)
  return(dvd_data)
}

ts = function(threshold, data) {
  data = as.numeric(unlist(data))
  
  ts_num = as.numeric(threshold)
  
  ts_text = unlist(strsplit(threshold, ""))
  
  ts_final = NULL
  
  if(is.na(ts_num) == TRUE)
    ts_final = quantile(data, probs = 0.75)
  else if(ts_text[length(ts_text)] != "%")
    ts_final = ts_num
  else{
    ts_num = ts_num/100
    
    ts_final = quantile(data, probs = ts_num)
  }
  
  return(ts_final)
}