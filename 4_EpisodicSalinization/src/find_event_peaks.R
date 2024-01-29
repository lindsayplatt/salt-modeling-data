# Original code from:
# https://github.com/galengorski/EventPicker/blob/master/appCode/event_picker_functions.R

# df <- read.csv('05_EventPickerApp/demo_files/SacCityHydrograph.csv')
# timestamp <- 'Date'
# plot_var <- 'X_00060_00003'
# sb_pk_thresh <- 0.000001
# sf_pk_thresh <- 0
# event_rise <- 0.001
# peak_top_thresh <- 0.1
# event_length_thresh <- 1
# 
# df.peaks <- find.peaks(df, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh)#, event_rise, peak_top_thresh, event_length_thresh)
# 
# df.peaks.ls <- event.scanner(df.peaks,timestamp, plot_var, event_rise, peak_top_thresh, event_length_thresh)

#the find.peaks function should ingest a dataframe and output a dataframe with the peaks idenitified
find.peaks <- function(df, timestamp, plot_var, sb_pk_thresh, sf_pk_thresh){
  
  #convert to seconds
  df$Date <- as.POSIXlt(df$dateTime, format = '%m/%d/%y')
  df$Seconds <- as.numeric(df[,timestamp])
  ##-remove NAs
  df <- df[!is.na(df[,plot_var]),]
  ##-make sure plotting variable is greater than zero
  df <- df[df[,plot_var] > 0,]
  ##-create an empty column for the slope forward and backward
  df$slp.b <- rep(NA, length.out = nrow(df))
  df$slp.f <- rep(NA, length.out = nrow(df))
  
  for(i in 2:(nrow(df)-1)){
    ##-calculate the slope back one day
    df$slp.b[i] <- (df[,plot_var][i]-df[,plot_var][(i-1)])/(df$Seconds[i]-df$Seconds[(i-1)])
    ##-calculate the slope forward one day
    df$slp.f[i] <- (df[,plot_var][(i+1)]-df[,plot_var][i])/(df$Seconds[(i+1)]-df$Seconds[i])
  }
  
  ##-make a column for the peak of each event flagged by a change in derivative
  df$peak.flag <- rep(NA, length.out = nrow(df))
  ##-now flag those derivative changes
  for(i in 2:(nrow(df)-1)){
    ##-if the slope back is greater than some threshold and the slope forward is negative, flag it as a peak
    ##-default sb_pk_thresh 0.000001
    ##-default sf_pk_thresh 0
    if(df$slp.b[i]>sb_pk_thresh & df$slp.f[i]<sf_pk_thresh){
      df$peak.flag[i] <- 1
    }else{
      ##-otherwise don't
      df$peak.flag[i] <- 0
    }
  }
  
  ##-Flag the changes in derivatitives, events is the row of single site which have events
  events <- which(df$peak.flag == 1)
  ##-if there are no events return df
  if(length(events) == 0){
    return(df)
  }else{
    ##-within single site, make a column that will signal which observations belong to which peak
    ##-this will turn into the rising and falling limbs
    df$event.flag <- rep(NA, length.out = nrow(df))
    
    for(i in 1:length(events)){
      k <- events[i]
      ##-the while loop goes forward in time until the slope forward is no longer negative
      while(df$slp.f[k] <0){
        ##-and labels that with the event number (i) and makes it positive (+)
        df$event.flag[k] <- i
        k <- k +1
        
        ##-if the last row of single sites is an event peak then move on
        if(k == nrow(df)){
          break
        }else{
        }
      }
      
      ##-now step backward in time for the falling limb
      ##-starting with the point directly before the peak
      j <- events[i]-1
      ##-if it's the first two data points in the site don't bother
      if(j == 1|j == 0){
        next
      }else{
        ##-as you step backwards label the days with the event number (i) and make it negative (-)
        ##-this time to indicate it is the rising limb (before the peak)
        while(df$slp.b[j] > 0){
          df$event.flag[j] <- -1*i
          ##-label j-1 as part of the rising limb too because j-1 won't be grouped with the rising limb
          ##-if it has a negative slope on the next step going back
          df$event.flag[j-1] <- -1*i
          j <- j - 1
          ##-if i is 1,2 or 3 in the data frame forget about it
          if(j == 2| j == 1| j == 0){
            break
          }else{
          }
        }
      }
    }
  }
  return(df)
}


event.scanner <- function(df, timestamp, plot_var, event_rise, peak_top_thresh, event_length_thresh){
  ##-how many unique events are there
  events <- which(df$peak.flag == 1)
  ##-make a y variable that is normalized by the max in the dataset, this is used for creating an event threshold
  df$y.norm <- (df[,plot_var]-min(df[,plot_var], na.rm = T))/(max(df[,plot_var], na.rm = T)-min(df[,plot_var], na.rm = T))
  ##-for each event that passes the tests we will have a list of features describing it
  event.features <- list()
  for(j in 1:length(events)){
    ##-select event i and remove any NAs that migth have popped up
    temp.event <- df[abs(df$event.flag) == j & !is.na(df$event.flag),]
    if(nrow(temp.event[temp.event$event.flag < 0,]) == 0){
      next
    }else{
      ##-if the rising limb or the falling limb is zero days long we aren't interested
      if(nrow(temp.event[df$event.flag<0,])==0|nrow(temp.event[df$event.flag>0,])==0){
        next
      }else{
        ##-look at the rate of rise on the rising limb and use that to cull some events that don't rise sharply
        ##-use the normalized discharge so it is comparable for multiple sites
        event.dq <- temp.event[temp.event$event.flag>0,]$y.norm[1] - temp.event[temp.event$event.flag<0,]$y.norm[1]
        ##-calculate dt in days, both dq and dt are from the start of the event to the peak
        event.dt <- (temp.event[temp.event$event.flag>0,]$Seconds[1]/86400) - (temp.event[temp.event$event.flag<0,]$Seconds[1]/86400)
        ##-default event_rise = 0.001
        if(event.dq/event.dt < event_rise){
          next
        }else{
          ##-if there are any NAs within the event we aren't interested
          if(TRUE %in% is.na(temp.event[,plot_var])){
            next
          }else{
            ##-if the max plot_var is less than the peak_top_thresh which is normalized to the record then remove it
            ##-good default peak_top_thresh = 0.10
            if(max(temp.event$y.norm)<peak_top_thresh){
              next
            }else{
              ##-if the event is shorter a certain length we aren't interested
              ##-good default is 1 to make it sensitive
              if(nrow(temp.event)<event_length_thresh){
                next
              }else{
                ##-full event
                temp.event <- df[which(abs(df$event.flag) == j),]
                event.features[[j]] <- temp.event
              }
            }
            
          }
        }
      }
    }
  }
  event.features.nn <- plyr::compact(event.features)
  return(event.features.nn)
}