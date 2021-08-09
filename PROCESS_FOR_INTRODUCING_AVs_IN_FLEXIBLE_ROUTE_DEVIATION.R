
# This code is developed at the Spatial Planning and Transportation Engineering Group, 
# Department of Built Environment, School of Engineering at Aalto University, Finland

# The script is developed for R programming language.


rm(list = ls.str(mode = 'numeric'))

# I N P U T S =========================================================================================

# AREA DIMENSIONS
W = 1.5 # service area width (km)
L = 5 # service area length (km)

# DEMAND CHARACTERISTICS
a = 0.5 # percent of passengers that will be served c-t-c with flex region
p_AV = 0.5 # percentage of passengers willing to use AVs
p_HD = 1-p_AV # percentage of passengers served by HDs
Q = 5 # demand density per direction (pax/sq.km/h)

# OPERATIONAL CHARACTERISTICS
V_HD = 30 # vehicle speed (km/h)
V_AV = 15 # vehicle speed (km/h)
Vwk = 5 # walking speed (km/hr)

tau_f = 0.008 # dwell time at fixed stops (hr/stop)
tau_r = 0.005 # dwell time at curb-to-curb (hr/stop)
tau_t = 0.010 # dwell time at terminal (hr/terminal)

O = 8 # hours of operation per day (hr)

# ECONOMIC FACTORS
# For Users
a_wk = 15 # walking cost coefficient (Eur/hr)
a_wt = 10 # waiting cost coefficient (Eur/hr)
a_r_HD = 7.5 # riding cost coefficient (Eur/hr)
a_r_AV = 5 # riding cost coefficient (Eur/hr)

#For Operations
c0_AV = 12 # base parameter of the unit operator cost [Eur/veh-hr] 
c1_AV = 0.28 # marginal cost of vehicle capacity [Eur/veh-hr]          
c0_HD = 18 # base parameter of the unit operator cost [Eur/veh-hr]  
c1_HD = 0.25 # marginal cost of vehicle capacity [Eur/veh-hr]         

#DEFINE WHICH OPERATIONAL STRATEGY (CASE) YOU WANT TO INVESTIGATE

# Possible answers
# 'A' (both types of vehicles operate with the same headway and different stop spacings)
# 'B' (both types of vehicles operate with different headways and the same stop spacing)
# 'C' (both types of vehicles operate with different headways and different stop spacings)
# 'D' (both types of vehicles operate with the same headways and the same stop spacing)

CASE = 'C'

# PROCESS IMPLEMENTATION ================================================================================

if (CASE == 'A') {

#=========================================================================================================
# C A S E A ==============================================================================================
#=========================================================================================================

# MATRIX INITIALIZATION ==================================================================================

H = matrix(0,(O*100),1)

Delta = matrix(0,(O*100),1)

val = matrix(0,(O*100),1)

# OPERATIONAL CHARACTERISTICS =============================================================================

# OPTIMAL SERVICE HEADWAY AND STOP SPACING ================================================================

for (i in 1:(O*100)) {

H[i] = i/100

S_AV_part1 = 2*Vwk*tau_f
S_AV_part2 = (2*c0_AV/H[i] + p_AV*Q*W*L*(2*c1_AV + a_r_AV))
S_AV_part3 = (1-a)*a_wk*p_AV*Q*W

S_AV = sqrt(S_AV_part1*S_AV_part2/S_AV_part3)

S_HD_part1 = 2*Vwk*tau_f
S_HD_part2 = (2*c0_HD/H[i] + p_HD*Q*W*L*(2*c1_HD + a_r_HD))
S_HD_part3 = (1-a)*a_wk*p_HD*Q*W

S_HD = sqrt(S_HD_part1*S_HD_part2/S_HD_part3)

H_part1 = (1/(Q*W*L))
H_part2 = 2*L*((c0_AV/V_AV)+(c0_HD/V_HD)) + 2*L*tau_f*((c0_AV/S_AV)+(c0_HD/S_HD)) + tau_f*(c0_AV + c0_HD)
H_part3 = a_wt/(Q*W*L)
H_part4 = a*(p_AV^2)*(W/V_AV + 2*tau_r)*(c1_AV + 0.5*a_r_AV) + a*(p_HD^2)*(W/V_HD + 2*tau_r)*(c1_HD + 0.5*a_r_HD)

val[i] = H_part1*sqrt(H_part2/(H_part3+H_part4))

Delta[i] = H[i] - val[i]

}

if (is.infinite(val[which.min(abs(Delta))])){

H_optimal = O

}else{

H_optimal=H[which.min(abs(Delta))]

}


S_AV_part1 = 2*Vwk*tau_f
S_AV_part2 = (2*c0_AV/H_optimal + p_AV*Q*W*L*(2*c1_AV + a_r_AV))
S_AV_part3 = (1-a)*a_wk*p_AV*Q*W
 
S_optimal_AV = S_AV = sqrt(S_AV_part1*S_AV_part2/S_AV_part3)

if (is.infinite(S_optimal_AV)){

S_optimal_AV = 2*L

}

S_HD_part1 = 2*Vwk*tau_f
S_HD_part2 = (2*c0_HD/H_optimal + p_HD*Q*W*L*(2*c1_HD + a_r_HD))
S_HD_part3 = (1-a)*a_wk*p_HD*Q*W
 
S_optimal_HD = S_HD = sqrt(S_HD_part1*S_HD_part2/S_HD_part3)

if (is.infinite(S_optimal_HD)){

S_optimal_HD = 2*L

}

# OPTIMAL PASSENGER CAPACITY ===============================================================================

K_AV = p_AV*Q*W*L*H_optimal
K_HD = p_HD*Q*W*L*H_optimal

# CYCLE TIME ===============================================================================================

C_AV = 2*L/V_AV + 2*tau_f*L/S_AV + a*p_AV*Q*(W^2)*L*H_optimal/V_AV + 2*a*p_AV*Q*W*L*H_optimal*tau_r + tau_t
C_HD = 2*L/V_HD + 2*tau_f*L/S_HD + a*p_HD*Q*(W^2)*L*H_optimal/V_HD + 2*a*p_HD*Q*W*L*H_optimal*tau_r + tau_t

# REQUIRED FLEET SIZE ======================================================================================

M_AV = C_AV/H_optimal
M_HD = C_HD/H_optimal

# ==========================================================================================================

# OPERATIONAL COSTS ========================================================================================

Operational_Costs_AV = (c0_AV + c1_AV*K_AV)*M_AV*O
Operational_Costs_HD = (c0_HD + c1_HD*K_HD)*M_HD*O

Total_Operational_Costs = Operational_Costs_AV + Operational_Costs_HD

# ==========================================================================================================

# USER COSTS ===============================================================================================

Walking_Costs_AV = (a_wk*2*p_AV*(1-a)*Q*W*L*H_optimal*(W+S_optimal_AV)/(4*Vwk))*(O/H_optimal)
Walking_Costs_HD = (a_wk*2*p_HD*(1-a)*Q*W*L*H_optimal*(W+S_optimal_HD)/(4*Vwk))*(O/H_optimal)
Total_Walking_Costs =  Walking_Costs_AV + Walking_Costs_HD

Waiting_Costs_AV = (a_wt*p_AV*Q*W*L*(H_optimal^2))*(O/H_optimal)
Waiting_Costs_HD = (a_wt*p_HD*Q*W*L*(H_optimal^2))*(O/H_optimal)
Total_Waiting_Costs =  Waiting_Costs_AV + Waiting_Costs_HD

Riding_Costs_AV = (a_r_AV*0.5*p_AV*Q*W*L*H_optimal*C_AV)*(O/H_optimal)
Riding_Costs_HD = (a_r_HD*0.5*p_HD*Q*W*L*H_optimal*C_HD)*(O/H_optimal)
Total_Riding_Costs = Riding_Costs_AV + Riding_Costs_HD

User_Costs_AV = Walking_Costs_AV + Waiting_Costs_AV + Riding_Costs_AV
User_Costs_HD = Walking_Costs_HD + Waiting_Costs_HD + Riding_Costs_HD
Total_User_Costs = User_Costs_AV + User_Costs_HD

# ==========================================================================================================

# GENERALIZED COSTS ========================================================================================

Generalized_Costs_AV = Operational_Costs_AV + User_Costs_AV
Generalized_Costs_HD = Operational_Costs_HD + User_Costs_HD

Total_Generalized_Costs = Generalized_Costs_AV + Generalized_Costs_HD

H_optimal_AV = H_optimal
H_optimal_HD = H_optimal

}

if (CASE == 'B') {

#==========================================================================
# C A S E B ===============================================================
#==========================================================================


# MATRIX INITIALIZATION ==================================================================================

S = matrix(0,(2*L*100),1)

Delta = matrix(0,(2*L*100),1)

val = matrix(0,(2*L*100),1)

# OPERATIONAL CHARACTERISTICS =============================================================================

# OPTIMAL SERVICE HEADWAY AND STOP SPACING ================================================================

for (i in 1:(2*L*100)) {

S[i] = i/100

H_AV_part1 = (1/(Q*W*L))
H_AV_part2 = 2*L*(c0_AV/V_AV) + 2*L*tau_f*(c0_AV/S[i]) + tau_f*c0_AV
H_AV_part3 = p_AV*a_wt/(Q*W*L)
H_AV_part4 = a*(p_AV^2)*(W/V_AV + 2*tau_r)*(c1_AV + 0.5*a_r_AV)

H_AV = H_AV_part1*sqrt(H_AV_part2/(H_AV_part3+H_AV_part4))

H_HD_part1 = (1/(Q*W*L))
H_HD_part2 = 2*L*(c0_HD/V_HD) + 2*L*tau_f*(c0_HD/S[i]) + tau_f*c0_HD
H_HD_part3 = p_HD*a_wt/(Q*W*L)
H_HD_part4 = a*(p_HD^2)*(W/V_HD + 2*tau_r)*(c1_HD + 0.5*a_r_HD)

H_HD = H_HD_part1*sqrt(H_HD_part2/(H_HD_part3+H_HD_part4))

S_part1 = 2*Vwk*tau_f
S_part2 = 2*c0_AV/H_AV + 2*c0_HD/H_HD 
S_part3 = Q*W*L*(2*(p_AV*c1_AV + p_HD*c1_HD) + (p_AV*a_r_AV + p_HD*a_r_HD))
S_part4 = (1-a)*a_wk*Q*W

val[i] = sqrt(S_part1*(S_part2+S_part3)/S_part4)

Delta[i] = S[i] - val[i]

}

if (is.infinite(val[which.min(abs(Delta))])){

S_optimal = 2*L

}else{

S_optimal=S[which.min(abs(Delta))]

}

H_AV_part1 = (1/(Q*W*L))
H_AV_part2 = 2*L*((c0_AV/V_AV)) + 2*L*tau_f*((c0_AV/S_optimal)) + tau_f*(c0_AV)
H_AV_part3 = p_AV*a_wt/(Q*W*L)
H_AV_part4 = a*(p_AV^2)*(W/V_AV + 2*tau_r)*(c1_AV + 0.5*a_r_AV)

H_optimal_AV = H_AV_part1*sqrt(H_AV_part2/(H_AV_part3+H_AV_part4))

H_HD_part1 = (1/(Q*W*L))
H_HD_part2 = 2*L*((c0_HD/V_HD)) + 2*L*tau_f*((c0_HD/S_optimal)) + tau_f*(c0_HD)
H_HD_part3 = p_HD*a_wt/(Q*W*L)
H_HD_part4 = a*(p_HD^2)*(W/V_HD + 2*tau_r)*(c1_HD + 0.5*a_r_HD)

H_optimal_HD = H_HD_part1*sqrt(H_HD_part2/(H_HD_part3+H_HD_part4))

if (is.infinite(H_optimal_AV)){

H_optimal_AV = O

}

if (is.infinite(H_optimal_HD)){

H_optimal_HD = O

}

# OPTIMAL PASSENGER CAPACITY ===============================================================================

K_AV = p_AV*Q*W*L*H_optimal_AV
K_HD = p_HD*Q*W*L*H_optimal_HD

# CYCLE TIME ===============================================================================================

C_AV = 2*L/V_AV + 2*tau_f*L/S_optimal + a*p_AV*Q*(W^2)*L*H_optimal_AV/V_AV + 2*a*p_AV*Q*W*L*H_optimal_AV*tau_r + tau_t
C_HD = 2*L/V_HD + 2*tau_f*L/S_optimal + a*p_HD*Q*(W^2)*L*H_optimal_HD/V_HD + 2*a*p_HD*Q*W*L*H_optimal_HD*tau_r + tau_t

# REQUIRED FLEET SIZE ======================================================================================

M_AV = C_AV/H_optimal_AV
M_HD = C_HD/H_optimal_HD

# ==========================================================================================================

# OPERATIONAL COSTS ========================================================================================

Operational_Costs_AV = (c0_AV + c1_AV*K_AV)*M_AV*O
Operational_Costs_HD = (c0_HD + c1_HD*K_HD)*M_HD*O

Total_Operational_Costs = Operational_Costs_AV + Operational_Costs_HD

# ==========================================================================================================

# USER COSTS ===============================================================================================

Walking_Costs_AV = (a_wk*2*p_AV*(1-a)*Q*W*L*H_optimal_AV*(W+S_optimal)/(4*Vwk))*(O/H_optimal_AV)
Walking_Costs_HD = (a_wk*2*p_HD*(1-a)*Q*W*L*H_optimal_HD*(W+S_optimal)/(4*Vwk))*(O/H_optimal_HD)
Total_Walking_Costs =  Walking_Costs_AV + Walking_Costs_HD

Waiting_Costs_AV = (a_wt*p_AV*Q*W*L*(H_optimal_AV^2))*(O/H_optimal_AV)
Waiting_Costs_HD = (a_wt*p_HD*Q*W*L*(H_optimal_HD^2))*(O/H_optimal_HD)
Total_Waiting_Costs =  Waiting_Costs_AV + Waiting_Costs_HD

Riding_Costs_AV = (a_r_AV*0.5*p_AV*Q*W*L*H_optimal_AV*C_AV)*(O/H_optimal_AV)
Riding_Costs_HD = (a_r_HD*0.5*p_HD*Q*W*L*H_optimal_HD*C_HD)*(O/H_optimal_HD)
Total_Riding_Costs = Riding_Costs_AV + Riding_Costs_HD

User_Costs_AV = Walking_Costs_AV + Waiting_Costs_AV + Riding_Costs_AV
User_Costs_HD = Walking_Costs_HD + Waiting_Costs_HD + Riding_Costs_HD
Total_User_Costs = User_Costs_AV + User_Costs_HD

# ==========================================================================================================

# GENERALIZED COSTS ========================================================================================

Generalized_Costs_AV = Operational_Costs_AV + User_Costs_AV
Generalized_Costs_HD = Operational_Costs_HD + User_Costs_HD

Total_Generalized_Costs = Generalized_Costs_AV + Generalized_Costs_HD

S_optimal_AV = S_optimal
S_optimal_HD = S_optimal

}


if (CASE == 'C') {

#==========================================================================
# C A S E C ===============================================================
#==========================================================================

# MATRIX INITIALIZATION ==================================================================================

H_AV = matrix(0,(O*100),1)

H_HD = matrix(0,(O*100),1)

S_AV = matrix(0,(2*L*100),1)

S_HD = matrix(0,(2*L*100),1)

Delta_AV = matrix(0,(O*100),1)

Delta_HD = matrix(0,(O*100),1)

val_AV = matrix(0,(O*100),1)

val_HD = matrix(0,(O*100),1)


# OPERATIONAL CHARACTERISTICS =============================================================================

# OPTIMAL SERVICE HEADWAY AND STOP SPACING ================================================================

for (i in 1:(O*100)) {

H_AV[i] = i/100

S_AV_part1 = 2*Vwk*tau_f
S_AV_part2 = (2*c0_AV/H_AV[i] + p_AV*Q*W*L*(2*c1_AV + a_r_AV))
S_AV_part3 = (1-a)*a_wk*p_AV*Q*W

S_AV = sqrt(S_AV_part1*S_AV_part2/S_AV_part3)

H_AV_part1 = (1/(Q*W*L))
H_AV_part2 = 2*L*(c0_AV/V_AV) + 2*L*tau_f*(c0_AV/S_AV) + tau_f*c0_AV
H_AV_part3 = p_AV*a_wt/(Q*W*L)
H_AV_part4 = a*(p_AV^2)*(W/V_AV + 2*tau_r)*(c1_AV + 0.5*a_r_AV)

val_AV[i] = H_AV_part1*sqrt(H_AV_part2/(H_AV_part3+H_AV_part4))

Delta_AV[i] = H_AV[i] - val_AV[i]

}

if (is.infinite(val_AV[which.min(abs(Delta_AV))])){

H_optimal_AV = O

}else{

H_optimal_AV=H_AV[which.min(abs(Delta_AV))]

}

S_AV_part1 = 2*Vwk*tau_f
S_AV_part2 = (2*c0_AV/H_optimal_AV + p_AV*Q*W*L*(2*c1_AV + a_r_AV))
S_AV_part3 = (1-a)*a_wk*p_AV*Q*W

S_optimal_AV = sqrt(S_AV_part1*S_AV_part2/S_AV_part3)

if (is.infinite(S_optimal_AV)){

S_optimal_AV = 2*L

}


for (i in 1:(O*100)) {

H_HD[i] = i/100

S_HD_part1 = 2*Vwk*tau_f
S_HD_part2 = (2*c0_HD/H_HD[i] + p_HD*Q*W*L*(2*c1_HD + a_r_HD))
S_HD_part3 = (1-a)*a_wk*p_HD*Q*W

S_HD = sqrt(S_HD_part1*S_HD_part2/S_HD_part3)

H_HD_part1 = (1/(Q*W*L))
H_HD_part2 = 2*L*(c0_HD/V_HD) + 2*L*tau_f*(c0_HD/S_HD) + tau_f*c0_HD
H_HD_part3 = p_HD*a_wt/(Q*W*L)
H_HD_part4 = a*(p_HD^2)*(W/V_HD + 2*tau_r)*(c1_HD + 0.5*a_r_HD)

val_HD[i] = H_HD_part1*sqrt(H_HD_part2/(H_HD_part3+H_HD_part4))

Delta_HD[i] = H_HD[i] - val_HD[i]

}

if (is.infinite(val_HD[which.min(abs(Delta_HD))])){

H_optimal_HD = O

}else{

H_optimal_HD=H_HD[which.min(abs(Delta_HD))]

}

S_HD_part1 = 2*Vwk*tau_f
S_HD_part2 = (2*c0_HD/H_optimal_HD + p_HD*Q*W*L*(2*c1_HD + a_r_HD))
S_AV_part3 = (1-a)*a_wk*p_HD*Q*W

S_optimal_HD = sqrt(S_HD_part1*S_HD_part2/S_HD_part3)

if (is.infinite(S_optimal_HD)){

S_optimal_HD = 2*L

}

# OPTIMAL PASSENGER CAPACITY ===============================================================================

K_AV = p_AV*Q*W*L*H_optimal_AV
K_HD = p_HD*Q*W*L*H_optimal_HD

# CYCLE TIME ===============================================================================================

C_AV = 2*L/V_AV + 2*tau_f*L/S_optimal_AV + a*p_AV*Q*(W^2)*L*H_optimal_AV/V_AV + 2*a*p_AV*Q*W*L*H_optimal_AV*tau_r + tau_t
C_HD = 2*L/V_HD + 2*tau_f*L/S_optimal_HD + a*p_HD*Q*(W^2)*L*H_optimal_HD/V_HD + 2*a*p_HD*Q*W*L*H_optimal_HD*tau_r + tau_t

# REQUIRED FLEET SIZE ======================================================================================

M_AV = C_AV/H_optimal_AV
M_HD = C_HD/H_optimal_HD

# ==========================================================================================================

# OPERATIONAL COSTS ========================================================================================

Operational_Costs_AV = (c0_AV + c1_AV*K_AV)*M_AV*O
Operational_Costs_HD = (c0_HD + c1_HD*K_HD)*M_HD*O

Total_Operational_Costs = Operational_Costs_AV + Operational_Costs_HD

# ==========================================================================================================

# USER COSTS ===============================================================================================

Walking_Costs_AV = (a_wk*2*p_AV*(1-a)*Q*W*L*H_optimal_AV*(W+S_optimal_AV)/(4*Vwk))*(O/H_optimal_AV)
Walking_Costs_HD = (a_wk*2*p_HD*(1-a)*Q*W*L*H_optimal_HD*(W+S_optimal_HD)/(4*Vwk))*(O/H_optimal_HD)
Total_Walking_Costs =  Walking_Costs_AV + Walking_Costs_HD

Waiting_Costs_AV = (a_wt*p_AV*Q*W*L*(H_optimal_AV^2))*(O/H_optimal_AV)
Waiting_Costs_HD = (a_wt*p_HD*Q*W*L*(H_optimal_HD^2))*(O/H_optimal_HD)
Total_Waiting_Costs =  Waiting_Costs_AV + Waiting_Costs_HD

Riding_Costs_AV = (a_r_AV*0.5*p_AV*Q*W*L*H_optimal_AV*C_AV)*(O/H_optimal_AV)
Riding_Costs_HD = (a_r_HD*0.5*p_HD*Q*W*L*H_optimal_HD*C_HD)*(O/H_optimal_HD)
Total_Riding_Costs = Riding_Costs_AV + Riding_Costs_HD

User_Costs_AV = Walking_Costs_AV + Waiting_Costs_AV + Riding_Costs_AV
User_Costs_HD = Walking_Costs_HD + Waiting_Costs_HD + Riding_Costs_HD
Total_User_Costs = User_Costs_AV + User_Costs_HD

# ==========================================================================================================

# GENERALIZED COSTS ========================================================================================

Generalized_Costs_AV = Operational_Costs_AV + User_Costs_AV
Generalized_Costs_HD = Operational_Costs_HD + User_Costs_HD

Total_Generalized_Costs = Generalized_Costs_AV + Generalized_Costs_HD

}

if (CASE == 'D') {

#=========================================================================================================
# C A S E D ==============================================================================================
#=========================================================================================================

# MATRIX INITIALIZATION ==================================================================================

H = matrix(0,(O*100),1)

Delta = matrix(0,(O*100),1)

val = matrix(0,(O*100),1)

# OPERATIONAL CHARACTERISTICS =============================================================================

# OPTIMAL SERVICE HEADWAY AND STOP SPACING ================================================================

for (i in 1:(O*100)) {

H[i] = i/100

S_part1 = 2*Vwk*tau_f
S_part2 = 2*c0_AV/H[i] + 2*c0_HD/H[i] 
S_part3 = Q*W*L*(2*(p_AV*c1_AV + p_HD*c1_HD) + (p_AV*a_r_AV + p_HD*a_r_HD))
S_part4 = (1-a)*a_wk*Q*W

S = sqrt(S_part1*(S_part2+S_part3)/S_part4)

H_part1 = (1/(Q*W*L))
H_part2 = 2*L*((c0_AV/V_AV)+(c0_HD/V_HD)) + 2*L*tau_f*((c0_AV/S)+(c0_HD/S)) + tau_f*(c0_AV + c0_HD)
H_part3 = a_wt/(Q*W*L)
H_part4 = a*(p_AV^2)*(W/V_AV + 2*tau_r)*(c1_AV + 0.5*a_r_AV) + a*(p_HD^2)*(W/V_HD + 2*tau_r)*(c1_HD + 0.5*a_r_HD)

val[i] = H_part1*sqrt(H_part2/(H_part3+H_part4))

Delta[i] = H[i] - val[i]

}

if (is.infinite(val[which.min(abs(Delta))])){

H_optimal = O

}else{

H_optimal=H[which.min(abs(Delta))]

}


S_part1 = 2*Vwk*tau_f
S_part2 = 2*c0_AV/H_optimal + 2*c0_HD/H_optimal
S_part3 = Q*W*L*(2*(p_AV*c1_AV + p_HD*c1_HD) + (p_AV*a_r_AV + p_HD*a_r_HD))
S_part4 = (1-a)*a_wk*Q*W

S_optimal = sqrt(S_part1*(S_part2+S_part3)/S_part4)

if (is.infinite(S_optimal)){

S_optimal = 2*L

}

# OPTIMAL PASSENGER CAPACITY ===============================================================================

K_AV = p_AV*Q*W*L*H_optimal
K_HD = p_HD*Q*W*L*H_optimal

# CYCLE TIME ===============================================================================================

C_AV = 2*L/V_AV + 2*tau_f*L/S_optimal + a*p_AV*Q*(W^2)*L*H_optimal/V_AV + 2*a*p_AV*Q*W*L*H_optimal*tau_r + tau_t
C_HD = 2*L/V_HD + 2*tau_f*L/S_optimal + a*p_HD*Q*(W^2)*L*H_optimal/V_HD + 2*a*p_HD*Q*W*L*H_optimal*tau_r + tau_t

# REQUIRED FLEET SIZE ======================================================================================

M_AV = C_AV/H_optimal
M_HD = C_HD/H_optimal

# ==========================================================================================================

# OPERATIONAL COSTS ========================================================================================

Operational_Costs_AV = (c0_AV + c1_AV*K_AV)*M_AV*O
Operational_Costs_HD = (c0_HD + c1_HD*K_HD)*M_HD*O

Total_Operational_Costs = Operational_Costs_AV + Operational_Costs_HD

# ==========================================================================================================

# USER COSTS ===============================================================================================

Walking_Costs_AV = (a_wk*2*p_AV*(1-a)*Q*W*L*H_optimal*(W+S_optimal)/(4*Vwk))*(O/H_optimal)
Walking_Costs_HD = (a_wk*2*p_HD*(1-a)*Q*W*L*H_optimal*(W+S_optimal)/(4*Vwk))*(O/H_optimal)
Total_Walking_Costs =  Walking_Costs_AV + Walking_Costs_HD

Waiting_Costs_AV = (a_wt*p_AV*Q*W*L*(H_optimal^2))*(O/H_optimal)
Waiting_Costs_HD = (a_wt*p_HD*Q*W*L*(H_optimal^2))*(O/H_optimal)
Total_Waiting_Costs =  Waiting_Costs_AV + Waiting_Costs_HD

Riding_Costs_AV = (a_r_AV*0.5*p_AV*Q*W*L*H_optimal*C_AV)*(O/H_optimal)
Riding_Costs_HD = (a_r_HD*0.5*p_HD*Q*W*L*H_optimal*C_HD)*(O/H_optimal)
Total_Riding_Costs = Riding_Costs_AV + Riding_Costs_HD

User_Costs_AV = Walking_Costs_AV + Waiting_Costs_AV + Riding_Costs_AV
User_Costs_HD = Walking_Costs_HD + Waiting_Costs_HD + Riding_Costs_HD
Total_User_Costs = User_Costs_AV + User_Costs_HD

# ==========================================================================================================

# GENERALIZED COSTS ========================================================================================

Generalized_Costs_AV = Operational_Costs_AV + User_Costs_AV
Generalized_Costs_HD = Operational_Costs_HD + User_Costs_HD

Total_Generalized_Costs = Generalized_Costs_AV + Generalized_Costs_HD

H_optimal_AV = H_optimal
H_optimal_HD = H_optimal
S_optimal_AV = S_optimal
S_optimal_HD = S_optimal

}


# ==========================================================================================================

# DISPLAY RESULTS ==========================================================================================

cat("The AV optimal service headway is", H_optimal_AV,"h/veh")

cat("The HD optimal service headway is", H_optimal_HD,"h/veh")

cat("The AV optimal stop spacing is", S_optimal_AV, "km/stop")

cat("The HD optimal stop spacing is", S_optimal_HD, "km/stop")

cat("The optimal AV passenger capacity is", K_AV, "pax/veh")

cat("The optimal HD passenger capacity is", K_HD, "pax/veh")

cat("The required AV fleet size is", M_AV, "veh")

cat("The required HD fleet size is", M_HD, "veh")

cat("The AV operational costs are", Operational_Costs_AV, "Euro/day")

cat("The HD operational costs are", Operational_Costs_HD, "Euro/day")

cat("The AV user costs are", User_Costs_AV, "Euro/day")

cat("The HD user costs are", User_Costs_HD, "Euro/day")
