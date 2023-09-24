# all of the data is now in the array
# write diagnosis into arrays MyRawData and MyReportData
# run through the array and fill in the diagnosis

ActiveCol <- lCol + 2

for (i in fRow:lRow) {
  for (x in fCol:lCol) {
    pFocal <- MyRawData[i, x]$pClass
    pFocalVal <- MyRawData[i, x]$Presence
    sFocal <- MyRawData[i, x]$cClass
    sFocalVal <- MyRawData[i, x]$Cover
    fCover <- MyRawData[i, x]$Cover
    
    # fill dopts variables
    if (DOpts$ShowCalcSheet) {
      DOptsSpp <- MyRawData[i, x]$spp
      DOptsFocalCol <- MyRawData[i, x]$FocalCol
      DOptsUnit <- MyRawData[i, x]$Unit
      DOptsCover <- MyRawData[i, x]$Cover
      DOptscClass <- MyRawData[i, x]$cClass
      DOptsPresence <- MyRawData[i, x]$Presence
      DOptspClass <- MyRawData[i, x]$pClass
    }
    
    for (y in fCol:lCol) {
      # fill "y" variables
      # we could get a slight performance increase by putting this after the next block where we ask is x <> y
      pTarget <- MyRawData[i, y]$pClass
      pTargetVal <- MyRawData[i, y]$Presence
      sTarget <- MyRawData[i, y]$cClass
      sTargetVal <- MyRawData[i, y]$Cover
      tCover <- MyRawData[i, y]$Cover
      MyRawData[i, y]$FocalCol <- x
      FocalUnit <- MyRawData[i, y]$Unit
      TargetUnit <- MyRawData[i, y]$CompareToUnit
      MyRawData[i, y]$TargetCol <- y
      Answer <- ""
      Score <- 0
      PotentialScore <- 0
      PotentialScoreSum <- 0
      
      # if units are different
      if (x != y) { # new unit combination
        Answer <- ""
        IsD <- FALSE
        IsDD <- FALSE
        PotentialAnswer <- ""
        pIsD <- FALSE
        pIsDD <- FALSE
        
        # add calc sheet data
        if (DOpts$ShowCalcSheet) {
          DOptsTargetCol <- y
          DOptsCompareToUnit <- MyRawData[i, y]$Unit
          DOptsCoverT <- MyRawData[i, y]$Cover
          DOptscClassT <- MyRawData[i, y]$cClass
          DOptsPresenceT <- MyRawData[i, y]$Presence
          DOptspClassT <- MyRawData[i, y]$pClass
        }
        
        # get differential
        Score <- 0
        Score <- DifferentialScore(pFocalVal, pTargetVal)
        Answer <- Differential(pFocalVal, pTargetVal)
        if (Answer == "d1" || Answer == "d2" || Answer == "d3") {
          IsD <- TRUE
        } else {
          IsD <- FALSE
        }
        
        # get potential differential
        PotentialScore <- DifferentialScore(pFocalVal, 0)
        PotentialAnswer <- Differential(pFocalVal, 0)
        if (PotentialAnswer == "d1" || Answer == "d2" || Answer == "d3") {
          pIsD <- TRUE
        } else {
          pIsD <- FALSE
        }
        PotentialScoreSum <- PotentialScoreSum + PotentialScore
        
        # get dominant differential
        if (GetDomDifferential(fCover, tCover, Cells(i, 2), x, y, MyRawData[i, x]$Presence) != "") {
          IsDD <- TRUE
          if (Answer != "") {
            Answer <- paste0(Answer, ", ", GetDomDifferential(fCover, tCover, Cells(i, 2), x, y, MyRawData[i, x]$Presence))
          } else {
            Answer <- GetDomDifferential(fCover, tCover, Cells(i, 2), x, y, MyRawData[i, x]$Presence)
          }
        }
        Score <- Score + ScoreDDC
        # if (DOptsSpp == "Picea mariana") {
        #   MsgBox(paste0(fCover, " - ", tCover, " - ", Answer))
        # }
        
        # get dominant differential potential
        # tmpStr <- GetDomDifferential(fCover, 0, Cells(i, 2), x, y, MyRawData[i, x]$Presence)
        tmpStr <- GetDomDifferentialPotentialScore(fCover, 0, Cells(i, 2), x, y, MyRawData[i, x]$Presence)
        if (tmpStr != "") {
          pIsDD <- TRUE
          PotentialScoreSum <- PotentialScoreSum + ScoreDDC
          if (PotentialAnswer != "") {
            PotentialAnswer <- paste0(PotentialAnswer, ", ", tmpStr)
          } else {
            PotentialAnswer <- tmpStr
          }
        }
        
        # get constant dominant
        if (DoConstantSpp) {
          MyCD <- GetConstDominant(pFocalVal, sFocal)
          MyCDscore <- GetConstDominantScore(pFocalVal, sFocal)
          if (MyCD != "") {
            IsConstant <- TRUE
            if (Answer > "") {
              Answer <- paste0(Answer, ", ", MyCD)
            } else {
              Answer <- MyCD
            }
          }
          Score <- Score + MyCDscore
        }
        
        # get constant dominant potential
        if (DoConstantSpp) {
          tmpStr <- GetConstDominant(pFocalVal, sFocal)
          if (tmpStr > "") {
            PotentialScore <- GetConstDominantScore(pFocalVal, 0)
            PotentialScoreSum <- PotentialScoreSum + PotentialScore
            if (PotentialAnswer != "") {
              PotentialAnswer <- paste0(PotentialAnswer, ", ", tmpStr)
            } else {
              PotentialAnswer <- tmpStr
            }
          }
        }
        
        # get constant
        if (DoConstantSpp) {
          MyC <- GetConstant(pFocalVal, sFocal)
          MyCscore <- GetConstantScore(pFocalVal, sFocal)
          IsConstant <- TRUE
          if (MyC > "") {
            if (Answer > "") {
              Answer <- paste0(Answer, ", ", MyC)
            } else {
              Answer <- MyC
            }
          }
          Score <- Score + MyCscore
        }
        
        # get constant potential
        if (DoConstantSpp) {
          tmpStr <- GetConstant(pFocalVal, sFocal)
          PotentialScore <- GetConstantScore(pFocalVal, 0)
          if (tmpStr > "") {
            if (PotentialAnswer > "") {
              PotentialAnswer <- paste0(PotentialAnswer, ", ", tmpStr)
            } else {
              PotentialAnswer <- tmpStr
            }
          }
          PotentialScoreSum <- PotentialScoreSum + PotentialScore
        }
        
        # get constant minor
        if (DoConstantSpp) {
          MyCM <- GetConstMinor(pFocalVal, sFocal)
          MyCMscore <- GetConstMinorScore(pFocalVal, sFocal)
          if (MyCM > "") {
            if (Answer > "") {
              Answer <- paste0(Answer, ", ", MyCM)
            } else {
              Answer <- MyCM
            }
          }
          Score <- Score + MyCMscore
        }
        
        # get constant minor potential
        if (DoConstantSpp) {
          tmpStr <- GetConstMinor(pFocalVal, sFocal)
          if (tmpStr > "") {
            PotentialScore <- GetConstMinorScore(pFocalVal, 0)
            PotentialScoreSum <- PotentialScoreSum + PotentialScore
            if (PotentialAnswer > "") {
              PotentialAnswer <- paste0(PotentialAnswer, ", ", tmpStr)
            } else {
              PotentialAnswer <- tmpStr
            }
          }
        }
        
        # if the answer is not null, write the answer and other data into the array MyRawData
        if (Answer != "") {
          MyRawData[i, y]$Diagnosis <- Answer
          MyRawData[i, y]$CompareToUnit <- Cells(2, ActiveCol)
          Cells(i, ActiveCol) <- Answer
          
          # write data into the report summary array MyReportData
          z <- z + 1
          MyReportData[z] <- list(ID = z,
                                  Lifeform = Cells(i, 1),
                                  spp = Cells(i, 2),
                                  Unit = Cells(5, ActiveCol),
                                  Diagnosis = Answer)
          
          # optionally, adjust the total points by multiplying by single decimal rounded constancy
          if (ScoreClass$AdjustPairwiseByConstancy) {
            Score <- Score * round(pFocalVal, 1)
            PotentialScoreSum <- PotentialScoreSum * round(pFocalVal, 1)
          }
          
          # add data to calc sheet
          if (DOpts$ShowCalcSheet) {
            Sheets("Calc")$Cells(CalcRow, 23) <- Score
            Sheets("Calc")$Cells(CalcRow, 24) <- IsD
            Sheets("Calc")$Cells(CalcRow, 25) <- IsDD
          }
          
          # if both differential and dominant differential then adjust the score
          if (IsD && IsDD) {
            AdjustScore <- 0
            AdjustScore <- sFocalVal / 10
            if (AdjustScore > 1) {
              AdjustScore <- 1
            }
            AdjustScore <- 2 * pFocalVal * AdjustScore
            Score <- Score + AdjustScore
          }
          # Score <- Score * pFocalVal
          
          # if potential differential and potential dominant differential then adjust the score
          if (pIsD && pIsDD) {
            AdjustScore <- 0
            AdjustScore <- sFocalVal / 10
            if (AdjustScore > 1) {
              AdjustScore <- 1
            }
            AdjustScore <- 2 * pFocalVal * AdjustScore
            PotentialScoreSum <- PotentialScoreSum + AdjustScore
          }
          PotentialScoreSum <- PotentialScoreSum * pFocalVal
        }
      }
    }
  }
}
