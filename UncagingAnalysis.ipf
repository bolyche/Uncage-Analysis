#pragma rtGlobals=3		// Use modern global access method and strict wave access.


//*******************************************************************************************************************************

Function PopupChoice ()

		String tracename
		Variable Choose = 4
		Variable/G returnvar
		Prompt traceName,"Trace",popup,TraceNameList("",";",1)
		Prompt Choose, "Is this fit good?", popup, "No: Do a Refit; No response: Save zero; Too noisy, save NaN; Yes: Save fit"
		DoPrompt "Goodness of Fit", Choose
		
		if (Choose == 1)
			returnvar = 0
			return returnvar
		elseif (Choose == 2)
			returnvar = 2
			return returnvar
		elseif (Choose  == 3)
			returnvar = 3
			return returnvar
		elseif (Choose  == 4)
			returnvar = 1
			return returnvar
		endif
		
End

//*******************************************************************************************************************************

Function PopupComment()

		Variable change = 2
		Variable/G returnchoice
		Prompt change, "Does this need to be reviewed later?", popup, "yes; no"
		String inputstring = "Any changes necessary?"
		DoPrompt/HELP="click cancel if nothing to add" inputstring, change
		
		if (change ==1)
			returnchoice = 1
			return returnchoice
		elseif (change ==2)
			returnchoice = 0
			return returnchoice
		endif
		
End

//*******************************************************************************************************************************

Function Refit()

		Variable better = 2
		Variable/G betterreturn
		Prompt better, "Better?", popup, "yes; no"
		String inputstring = "Improved Fit"
		DoPrompt/HELP="if you click cancel it will return yes and save" inputstring, better
		
		if (better ==1)
			betterreturn = 1
			return betterreturn
		elseif (better ==2)
			betterreturn = 0
			return betterreturn
		endif
		
End

//*******************************************************************************************************************************

Function UserCursorAdjust_ContButtonProc(ctrlName) : ButtonControl
	String ctrlName

	DoWindow/K tmp_PauseforCursor			
End
		
//*******************************************************************************************************************************
//*******************************************************************************************************************************

Macro UncagingAnalysis (traceunc, uncageinterval, fitrange)
	String traceunc
	Variable uncageinterval=1, fitrange=0.02
	Prompt traceunc, "Response Trace", popup, WaveList("*", ";", "")
	Prompt uncageinterval, "Interval between uncaging points (s)"
	Prompt fitrange, "Fitting range (s)"

	String/G tracepower, new, new2, new3, newname		//Corresponding power trace saved by PrairieView as _2, while recording as _1
	new = RemoveEnding(traceunc)
	new2 = RemoveEnding(new)
	new3 = RemoveEnding(new2)
	tracepower = new3 + "2p1"
	
	Variable i=0
	Variable Td = 0.004							//Tau decay estimate
	Variable Tr = Td/2						//Tau rise estimate
	Variable offsetrange = 0.01					//10ms averaged for offset of trace
	Variable peakrange = 0.001				//1ms averaged for offset of peak
	
	Variable mini, maxi							//x point range for getting initial valuation of peak position
	Variable offsetpt1, offsetpt2				//Specified later: is the x value range for trace offset
	Variable minpeak, maxpeak					//Peak evaluation range for getting amplitude
	Variable/G amplit						//Variable to store amplitudes into wave
	Variable/G offsetfunction					//w[4] for offsetting trace in DiffTwoExp function
	Variable/G uncgpnt						//uncaging point and it's end
	Variable maximum							//maximum of each uncaging power pulse from trace
	
	Variable V_FitMaxIters = 100			//Number of iterations FitFunc can go through
	Variable/G Threshold						
	Variable/G V_FitError					//If there's a problem e.g. Singular Matrix Error, then saves specifies action (save NaN)
	Variable/G cursorA, cursorB				
	Make/O/D W_coef = NaN					//For holding DiffTwoExp coefficients	


//---------------------------------------------Start of main do-while-loop. Includes i as key incrementor
//--------------------------------------------- Gets all values for FunctFit and stores

//---------------------------------------------First check / create all waves
	
	if (!WaveExists(root:Refwave))
		Make/O/T/N=1 Refwave
		DeletePoints 0, 1,  Refwave
	endif	

	
	if (!WaveExists(root:amplitudedata))
		Make/O/N=1 AmplitudeData
		DeletePoints 0, 1, amplitudedata
	endif
	
	
	if (!WaveExists(root:T0Data))
		Make/O/N=1 T0Data
		DeletePoints 0, 1, T0Data
	endif
	
	
	if (!WaveExists(root:TauDecayData))
		Make/O/N=1 TauDecayData
		DeletePoints 0, 1, TauDecayData
	endif
	
	
	if (!WaveExists(root:TauRiseData))
		Make/O/N=1 TauRiseData
		DeletePoints 0, 1, TauRiseData
	endif		
	

	if (!WaveExists(root:uncgpntData))
		Make/O/N=1 uncgpntData
		DeletePoints 0, 1,  uncgpntData
	endif	


	if (!WaveExists(root:TimeDiffUncgpnt_Response))
		Make/O/N=1 TimeDiffUncgpnt_Response
		DeletePoints 0, 1,  TimeDiffUncgpnt_Response
	endif	
			
	
	if ( !WaveExists(root:OffsetData))		//this is y0
		Make/O/N=1 OffsetData
		DeletePoints 0, 1, OffsetData
	endif
	

	if ( !WaveExists(root:AmplitConfidenceLevel))
		Make/O/N=1 AmplitConfidenceLevel
		DeletePoints 0, 1, AmplitConfidenceLevel
	endif
	

	if ( !WaveExists(root:commentwav))
		Make/O/T/N=1 commentwav
		DeletePoints 0, 1, commentwav
	endif
	
//--------------------------------Start loop

	do

	V_FitError = 0
		
//--------------------------------Set uncaging point

		threshold = 0.7*maximum
		maximum = WaveMax($tracepower, i*uncageinterval, ((i+1)*uncageinterval))							//Assumes first uncaging point within 0 to uncage-interval set by user
		
		if (i >= 1)
			if	(maximum < threshold)		//If maximum is smaller than 5 standard deviations of the error, then break
				break
			endif
		endif
		
		
			
		FindPeak/Q/B=1/M=(maximum*0.9)/R=(i*uncageinterval, ((i+1)*uncageinterval)) $tracepower		//finds the maximum of each pulse
			
		uncgpnt = V_LeadingEdgeLoc

		InsertPoints numpnts (uncgpntData), 1, uncgpntData
		uncgpntData[numpnts(uncgpntData)] = uncgpnt	
		

//--------------------------------Obtain offset and amplitude

		Duplicate/O $traceunc, smoothed
		Smooth/B=7 5, smoothed
		
		mini = uncgpnt - (fitrange/2)									//variables for initial evaluation
		maxi = uncgpnt + fitrange
		
			Wavestats/Q/R=(mini, maxi) smoothed						//find minimum
			
			offsetpt1= uncgpnt  - offsetrange							//variables for offset range
			offsetpt2 = uncgpnt

			minpeak = V_minloc - (peakrange/2)						//variables for peak range
			maxpeak = V_minloc + (peakrange/2)
	
				Wavestats/Q/R=(offsetpt1, offsetpt2) $traceunc		//find offset
				offsetfunction = V_Avg

				Wavestats/Q/R=(minpeak, maxpeak) $traceunc		//find peak		
				amplit = abs(abs(V_avg)-abs(offsetfunction))								//amplitude = average y-val of peak
										
//-------------------------------------Difference of Two Exponentials Function Fitting

		Duplicate/O $traceunc fit
		W_coef = {(-amplit), uncgpnt, Td, Tr, offsetfunction}
		FuncFit/N/Q/H="00000" /NTHR=0 DiffTwoExp2 W_coef  $traceunc((uncgpnt-0.01),(uncgpnt+fitrange))/D=fit /F = {0.95, 4}
		
		if ( V_FitError)
			W_coef = NaN
		endif
		
		//if problem with fit, likely uncgpnt not correctly set, or trace has problems fitting: this should be elimated with V_FitError however

//-------------------------------------Name graphs and Display
		
		String/G fitwave = nameofwave($traceunc) + "_u" + num2str(i+1)	
		String/G tracecopy = nameofwave($traceunc) + "_P" + num2str(i+1)
		Duplicate/O/R=(uncgpnt,(uncgpnt+fitrange)) fit, $fitwave
		Duplicate/O/R=((uncgpnt-0.01),(uncgpnt+(2*fitrange))) $traceunc, $tracecopy		//Trace saved and displayed as 10ms before uncaging, and 2*fitrange after
		
		Display/N=Checking $tracecopy
		AppendToGraph /C = (0,0,0)$fitwave
		Cursor A $tracecopy, uncgpnt
		ModifyGraph/W=Checking expand=2
		
		if (W_coef[0] <= (-25e-12)) 
			SetAxis/W=Checking left (offsetfunction-60e-12), (offsetfunction+10e-12) 
		else
			SetAxis/W=Checking left (offsetfunction-30e-12), (offsetfunction+10e-12)
		endif
		
//---------------------------------------------Start of if-loop popup menu: whether to save data=good fits or NaN=bad fits
//---------------------------------------------

		PopupChoice ()
		
			if (returnvar == 1)	//yes good fit
		
		
//-------------------------------------
				
				
					InsertPoints numpnts (amplitudeData), 1, amplitudeData
					amplitudeData[numpnts(amplitudeData)] = W_coef[0]		
							
		
//-------------------------------------

					InsertPoints numpnts (T0Data), 1, T0Data
					T0Data[numpnts(T0Data)] = W_coef[1]				
	

//-------------------------------------
				
					InsertPoints numpnts (TauDecayData), 1, TauDecayData
					TauDecayData[numpnts(TauDecayData)] = W_coef[2]		

		
//-------------------------------------
			
					InsertPoints numpnts (TauRiseData), 1,TauRiseData
					TauRiseData[numpnts(TauRiseData)] = W_coef[3]		
				
				
//-------------------------------------

					InsertPoints numpnts (OffsetData), 1, OffsetData
					OffsetData[numpnts(OffsetData)] = W_coef[4]	
					
					
//-------------------------------------
					
					InsertPoints numpnts (TimeDiffUncgpnt_Response), 1, TimeDiffUncgpnt_Response
					TimeDiffUncgpnt_Response[numpnts(TimeDiffUncgpnt_Response)] = ((W_coef[1]) - uncgpnt)	


//-------------------------------------

					InsertPoints numpnts (AmplitConfidenceLevel), 1, AmplitConfidenceLevel
					AmplitConfidenceLevel[numpnts(AmplitConfidenceLevel)] = W_ParamConfidenceInterval[0]		//amplitude confidence from FuncFit


//-------------------------------------
				
					InsertPoints numpnts (Refwave), 1, Refwave
					newname  = nameofwave($traceunc) + "_P" + num2str(i+1)
					Refwave[numpnts(Refwave)] = newname	
				
				
//-------------------------------------
				
					InsertPoints numpnts (commentwav), 1, commentwav
									
					PopupComment()
					if (returnchoice == 1)
									commentwav[numpnts(commentwav)] = "Review this"
					else if (returnchoice == 0)
									commentwav[numpnts(commentwav)] = ""
					endif

//-------------------------------------

		endif

//-------------------------------------

		if (returnvar == 2)		//Nothing there: save as zero for no response
		
					//-------------------------------------
				
								
					InsertPoints numpnts (amplitudeData), 1, amplitudeData
					amplitudeData[numpnts(amplitudeData)] = 0		
							
		
					//-------------------------------------

					InsertPoints numpnts (T0Data), 1, T0Data
					T0Data[numpnts(T0Data)] = 0			
	

					//-------------------------------------
				
					InsertPoints numpnts (TauDecayData), 1, TauDecayData
					TauDecayData[numpnts(TauDecayData)] = 0		

		
					//-------------------------------------
			
					InsertPoints numpnts (TauRiseData), 1,TauRiseData
					TauRiseData[numpnts(TauRiseData)] = 0	


					//-------------------------------------

					InsertPoints numpnts (OffsetData), 1, OffsetData
					OffsetData[numpnts(OffsetData)] = 0		
					
					
					//-------------------------------------
					
					InsertPoints numpnts (TimeDiffUncgpnt_Response), 1, TimeDiffUncgpnt_Response
					TimeDiffUncgpnt_Response[numpnts(TimeDiffUncgpnt_Response)] = 0
					
					
					//-------------------------------------
				
					InsertPoints numpnts (AmplitConfidenceLevel), 1, AmplitConfidenceLevel
					AmplitConfidenceLevel[numpnts(AmplitConfidenceLevel)] = 0
				
				
					//-------------------------------------
				
					InsertPoints numpnts (Refwave), 1, Refwave
					String/G newname = nameofwave($traceunc) + "_P" + num2str(i+1)
					Refwave[numpnts(Refwave)] = newname	
				
				
					//-------------------------------------
				
					InsertPoints numpnts (commentwav), 1, commentwav
									
					PopupComment()
					if (returnchoice == 1)
									commentwav[numpnts(commentwav)] = "Review this"
					else if (returnchoice == 0)
									commentwav[numpnts(commentwav)] = ""
					endif
					

					//-------------------------------------
		
		endif


//-------------------------------------

		if (returnvar == 3)		//Nothing there: noise! Save NaN
		
					//-------------------------------------
				
								
					InsertPoints numpnts (amplitudeData), 1, amplitudeData
					amplitudeData[numpnts(amplitudeData)] = NaN	
							
		
					//-------------------------------------

					InsertPoints numpnts (T0Data), 1, T0Data
					T0Data[numpnts(T0Data)] = NaN		
	

					//-------------------------------------
				
					InsertPoints numpnts (TauDecayData), 1, TauDecayData
					TauDecayData[numpnts(TauDecayData)] = NaN	

		
					//-------------------------------------
			
					InsertPoints numpnts (TauRiseData), 1,TauRiseData
					TauRiseData[numpnts(TauRiseData)] = NaN	


					//-------------------------------------

					InsertPoints numpnts (OffsetData), 1, OffsetData
					OffsetData[numpnts(OffsetData)] = NaN		
					
					
					//-------------------------------------
					
					InsertPoints numpnts (TimeDiffUncgpnt_Response), 1, TimeDiffUncgpnt_Response
					TimeDiffUncgpnt_Response[numpnts(TimeDiffUncgpnt_Response)] = NaN
					
					
					//-------------------------------------
				
					InsertPoints numpnts (AmplitConfidenceLevel), 1, AmplitConfidenceLevel
					AmplitConfidenceLevel[numpnts(AmplitConfidenceLevel)] = NaN
				
				
					//-------------------------------------
				
					InsertPoints numpnts (Refwave), 1, Refwave
					String/G newname = nameofwave($traceunc) + "_P" + num2str(i+1)
					Refwave[numpnts(Refwave)] = newname	
				
				
					//-------------------------------------
				
					InsertPoints numpnts (commentwav), 1, commentwav
									
					PopupComment()
					if (returnchoice == 1)
									commentwav[numpnts(commentwav)] = "Review this"
					else if (returnchoice == 0)
									commentwav[numpnts(commentwav)] = ""
					endif
					

					//-------------------------------------
		
		endif

//-------------------------------------
		
		if (returnvar == 0)		//No, not good fit. Get fit points from cursors

//---------------------------------------------

		DoWindow/K Checking
		
		Duplicate/O/R=((uncgpnt-(0.5*fitrange)),(uncgpnt+(2*fitrange))) $traceunc, $tracecopy
		Duplicate/O/R=(uncgpnt,(uncgpnt+fitrange)) fit, $fitwave
		Display/N=Checking $tracecopy
		AppendToGraph /C = (0,0,0)$fitwave
		ModifyGraph/W=Checking expand=2
		
		if (W_coef[0] <= (-25e-12))
			SetAxis/W=Checking left (offsetfunction-60e-12), (offsetfunction+10e-12) 
		else
			SetAxis/W=Checking left (offsetfunction-30e-12), (offsetfunction+10e-12)
		endif
		
		NewPanel/K=2 as "Pause for cursor"
		DoWindow/C tmp_PauseforCursor
		AutoPositionWindow/M=1/R=Checking
		
		DrawText 21,20,"Adjust the cursors and then"
		DrawText 21,40,"Click Continue."
		Button button0,pos={80,58},size={92,20},title="Continue"
		Button button0,proc=UserCursorAdjust_ContButtonProc
		
		ShowInfo/CP=0/W=Checking
		PauseForUser tmp_PauseforCursor, Checking
		
			if (strlen(CsrWave(A))>0 && strlen(CsrWave(B))>0)
				cursorA = xcsr(A)
				cursorB = xcsr(B)
			endif
		
		DoWindow/K Checking
		
//---------------------------------------------
		
			V_FitError = 0
			
			Duplicate/O $traceunc, fit
			W_coef = {(-amplit), (uncgpnt), Td, Tr, offsetfunction}
			FuncFit/N/Q/H="00000" /NTHR=0 DiffTwoExp2 W_coef  $traceunc(cursorA, cursorB)/D=fit /F = {0.95, 4}		//new fit with cursor x values
			
			if ( V_FitError)
				W_coef = NaN
			endif

//---------------------------------------------Display and check if good fit		
			
			String/G fitwave = nameofwave($traceunc) + "_u" + num2str(i+1)	
			String/G tracecopy = nameofwave($traceunc) + "_P" + num2str(i+1)
			Duplicate/O/R=(cursorA, cursorB) fit, $fitwave
			Duplicate/O/R=((uncgpnt-(0.5*fitrange)),(uncgpnt+(2*fitrange))) $traceunc, $tracecopy
		
			Display/N=Checking $tracecopy
			AppendToGraph /C = (0,0,0)$fitwave
			ModifyGraph/W=Checking expand=2
			
			if (W_coef[0] <= (-25e-12))
				SetAxis/W=Checking left (offsetfunction-60e-12), (offsetfunction+10e-12) 
			else
				SetAxis/W=Checking left (offsetfunction-30e-12), (offsetfunction+10e-12)
			endif
			
			SetAxis/W=Checking left (offsetfunction-30e-12), (offsetfunction+10e-12)
			SetAxis/W=Checking bottom (uncgpnt-(0.5*fitrange)),(uncgpnt+(2*fitrange))
		
			Refit()
			
				if (betterreturn ==  1)
				
				//-------------------------------------
				
				
					InsertPoints numpnts (amplitudeData), 1, amplitudeData
					amplitudeData[numpnts(amplitudeData)] = W_coef[0]		
							
		
					//-------------------------------------

					InsertPoints numpnts (T0Data), 1, T0Data
					T0Data[numpnts(T0Data)] = W_coef[1]				
	

					//-------------------------------------
				
					InsertPoints numpnts (TauDecayData), 1, TauDecayData
					TauDecayData[numpnts(TauDecayData)] = W_coef[2]		

		
					//-------------------------------------
			
					InsertPoints numpnts (TauRiseData), 1,TauRiseData
					TauRiseData[numpnts(TauRiseData)] = W_coef[3]
					
					
					//-------------------------------------

					InsertPoints numpnts (OffsetData), 1, OffsetData
					OffsetData[numpnts(OffsetData)] = W_coef[4]	
					
					//-------------------------------------
					
					InsertPoints numpnts (TimeDiffUncgpnt_Response), 1, TimeDiffUncgpnt_Response
					TimeDiffUncgpnt_Response[numpnts(TimeDiffUncgpnt_Response)] = (W_coef[1] - uncgpnt)		
					
					
					//-------------------------------------
				
					InsertPoints numpnts (AmplitConfidenceLevel), 1, AmplitConfidenceLevel
					AmplitConfidenceLevel[numpnts(AmplitConfidenceLevel)] = W_ParamConfidenceInterval[0]
				
				
					//-------------------------------------
				
					InsertPoints numpnts (Refwave), 1, Refwave
					String/G newname = nameofwave($traceunc) + "_P" + num2str(i+1)
					Refwave[numpnts(Refwave)] = newname	
				
				
					//-------------------------------------
				
					InsertPoints numpnts (commentwav), 1, commentwav
									
					PopupComment()
					if (returnchoice == 1)
									commentwav[numpnts(commentwav)] = "Review this"
					else if (returnchoice == 0)
									commentwav[numpnts(commentwav)] = ""
					endif


				//-------------------------------------If fit not good, saves in waves as NaN
				
				else if (betterreturn == 0)
				
				//-------------------------------------
				
								
					InsertPoints numpnts (amplitudeData), 1, amplitudeData
					amplitudeData[numpnts(amplitudeData)] = NaN		
							
		
					//-------------------------------------

					InsertPoints numpnts (T0Data), 1, T0Data
					T0Data[numpnts(T0Data)] = NaN			
	

					//-------------------------------------
				
					InsertPoints numpnts (TauDecayData), 1, TauDecayData
					TauDecayData[numpnts(TauDecayData)] = NaN		

		
					//-------------------------------------
			
					InsertPoints numpnts (TauRiseData), 1,TauRiseData
					TauRiseData[numpnts(TauRiseData)] = NaN		


					//-------------------------------------

					InsertPoints numpnts (OffsetData), 1, OffsetData
					OffsetData[numpnts(OffsetData)] = NaN
					
					
					//-------------------------------------
					
					InsertPoints numpnts (TimeDiffUncgpnt_Response), 1, TimeDiffUncgpnt_Response
					TimeDiffUncgpnt_Response[numpnts(TimeDiffUncgpnt_Response)] = NaN	
					
					
					//-------------------------------------
				
					InsertPoints numpnts (AmplitConfidenceLevel), 1, AmplitConfidenceLevel
					AmplitConfidenceLevel[numpnts(AmplitConfidenceLevel)] = NaN
				
				
					//-------------------------------------
				
					InsertPoints numpnts (Refwave), 1, Refwave
					String/G newname = nameofwave($traceunc) + "_P" + num2str(i+1)
					Refwave[numpnts(Refwave)] = newname	
				
				
					//-------------------------------------
				
					InsertPoints numpnts (commentwav), 1, commentwav
									
					PopupComment()
					if (returnchoice == 1)
									commentwav[numpnts(commentwav)] = "Review this"
					else if (returnchoice == 0)
									commentwav[numpnts(commentwav)] = ""
					endif
				
				//--------------------------------
				
				endif
				
//--------------------------------		
		
		endif
						
//--------------------------------Below: Save difference between uncaging point and FuncFit start, Close graph window
	
	
	DoWindow/K Checking
			
//---------------------------------------------End of if-loop popup menu (whether to save data or save NaN) 
//---------------------------------------------
	
		i += 1
		
while(1)
//	while ((i+1)< pnt2x($tracepower, numpnts($tracepower)))
	
//---------------------------------------------End of do-while-loop, i incremented by 1 to find next uncaging current
//--------------------------------------------- 
//---------------------------------------------Kill all global variables, waves (if they exist) and global strings
	
	KillWaves W_coef, smoothed, fit
	KillVariables/A
	KillStrings/A
	
	if (WaveExists(root:W_sigma))
		KillWaves W_sigma
	endif
	
	if (WaveExists(root:W_ParamConfidenceInterval))
		KillWaves W_ParamConfidenceInterval
	endif
	
	if (WaveExists(root:M_Jacobian))
		KillWaves M_Jacobian
	endif
	
EndMacro


//*******************************************************************************************************************************
//*******************************************************************************************************************************

//*******************************************************************************************************************************
//*******************************************************************************************************************************


Function DiffTwoExp2(w,t) : FitFunc
	Wave w
	Variable t

	//CurveFitDialog/ These comments were created by the Curve Fitting dialog. Altering them will
	//CurveFitDialog/ make the function less convenient to work with in the Curve Fitting dialog.
	//CurveFitDialog/ Equation:
	//CurveFitDialog/    IF    ((t-t0) < 0 )
	//CurveFitDialog/        f(t) = 0
	//CurveFitDialog/    ELSE
	//CurveFitDialog/        f(t) = (gsyn*(-exp(-(t-t0)/tr)+exp(-(t-t0)/td))/(-exp(-(t0+(td*tr/(td-tr))*ln(td/tr)-t0)/tr)+exp(-(t0+(td*tr/(td-tr))*ln(td/tr)-t0)/td)) -y0)
	//CurveFitDialog/    ENDIF
	//CurveFitDialog/ End of Equation
	//CurveFitDialog/ Independent Variables 1
	//CurveFitDialog/ t
	//CurveFitDialog/ Coefficients 5
	//CurveFitDialog/ w[0] = gsyn
	//CurveFitDialog/ w[1] = t0
	//CurveFitDialog/ w[2] = td
	//CurveFitDialog/ w[3] = tr
	//CurveFitDialog/ w[4] = y0
	   IF    ((t-w[1]) < 0 )
	       return w[4]
	   ELSE
	       return (w[4]+w[0]*(-exp(-(t-w[1])/w[3])+exp(-(t-w[1])/w[2]))/(-exp(-(w[1]+(w[2]*w[3]/(w[2]-w[3]))*ln(w[2]/w[3])-w[1])/w[3])+exp(-(w[1]+(w[2]*w[3]/(w[2]-w[3]))*ln(w[2]/w[3])-w[1])/w[2])))
	   ENDIF
End

//*******************************************************************************************************************************
