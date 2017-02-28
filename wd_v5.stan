data {
  
  int<lower=1> Nq;               // Number of quarters
  int<lower=1> Npeople;          // Number of people
  int          WD[Nq * Npeople]; // Withdrawal Indicators
}

// In this section we claculate any constant variables or arrays that we are going 
// to use in the code later.
// In this case we add: 
///    1. Constant Ntot, the total number of data points.
///    2. Array of quarter numbers with the first withdrawal for each person.
transformed data{
  int<lower=1> Ntot;                       // Total number of data points
  int  FirstWithdrawalQuarterAdj[Npeople]; // First Withdrawal Quarter. Adjusted
  
  Ntot = Nq * Npeople; 
  
  // Filling FirstWithdrawalQuarter array
  {
  
    int   current_pos;
    int   next_pos;
    int   current_q;
    int   first_wd_q;
    
    current_pos = 1;
    for(iperson in 1:Npeople){
        next_pos = current_pos + Nq - 1;
        
        current_q = 1;    // Current quarter
        first_wd_q = -1;  // First quarter of withdrawal
        
        //Loop over quarters
        for(j in current_pos:next_pos){
            if(WD[j] == 1){
                first_wd_q = current_q;
                break;
            }
            
            // Increment quarter
            current_q = current_q + 1;
        }
        
        // If no withdrawals, set this number high
        // That is the adjustion
        if(first_wd_q < 0){
            first_wd_q = Nq + 10;
        }
        
        // Finally, set it
        FirstWithdrawalQuarterAdj[iperson] = first_wd_q;
        
        //Update current position
        current_pos = next_pos + 1;
    }// end of loop over people
  }
}

// All model parameters definitions go here
parameters {
  // Define parameters to estimate
  real                coef_intercept;   // Mean base logit probability of withdrawal
  real                coef_q;           // Quarter coefficient
  real<lower=0.0001>  sigma;            // Standard deviation for the base logit probability. Can't be zero as Stan doesn't like it
  real                coef_Jump;        // Jump coefficient. Jump in the withdrawal probability after first withdrawal.
  vector[Npeople]     wd_prob_link;     // Base logit probability of withdrawal for each person (for quarter number 1)
}

// Here we calculate Link for each quarter - 
transformed parameters  {
    vector[Ntot] link;
    {
        int   current_pos;
        int   next_pos;
        real  current_link;
        int   current_q_m1;// Current quarter minus 1
        int   first_wd_q;
        
        current_pos = 1;
        for(iperson in 1:Npeople){
            next_pos     = current_pos + Nq - 1;
            current_link = wd_prob_link[iperson];
            
            current_q_m1 = 0; // Current quarter minus 1
            
            first_wd_q = FirstWithdrawalQuarterAdj[iperson];
            
            for(j in current_pos:next_pos){
                link[j] = current_link + current_q_m1 * coef_q + (current_q_m1 + 1 > first_wd_q) * coef_Jump;
                // Increment quarter
                current_q_m1 = current_q_m1 + 1;
            }
            
            //Update current position
            current_pos = next_pos + 1;
        }
    }
}

model {
  // Prior part of Bayesian inference (flat if unspecified)
  coef_intercept ~ normal(-2, 2);
  coef_q ~ normal(0, 0.1);
  sigma ~ normal(0, 1);
  coef_Jump ~ normal(0, 1.0);
  
  // Likelihood part of Bayesian inference
  wd_prob_link ~ normal(coef_intercept, sigma);
  
  WD ~ bernoulli_logit(link);
}

