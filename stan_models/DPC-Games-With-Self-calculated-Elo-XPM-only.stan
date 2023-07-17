data {
  int<lower=0> N_matches;
  int<lower=0, upper=1> radiant_win[N_matches];
  real radiant_team_XPM[N_matches];
  real radiant_elo[N_matches];
  real dire_team_XPM[N_matches];
  real dire_elo[N_matches];
}
parameters {
  real intercept;
  real b_radiant_team_XPM;
  real b_radiant_elo;
  real b_dire_team_XPM;
  real b_dire_elo;
}
model{
  for (i in 1:N_matches) {
    radiant_win[i] ~ bernoulli_logit(
      intercept
      + b_radiant_team_XPM * radiant_team_XPM[i]
      + b_radiant_elo * radiant_elo[i]
      + b_dire_team_XPM * dire_team_XPM[i]
      + b_dire_elo * dire_elo[i]
      );
  }
}
generated quantities {
  vector[N_matches] log_lik;
  
  for (i in 1:N_matches) {
    log_lik[i] = bernoulli_logit_lpmf(radiant_win[i] | 
      intercept
      + b_radiant_team_XPM * radiant_team_XPM[i]
      + b_radiant_elo * radiant_elo[i]
      + b_dire_team_XPM * dire_team_XPM[i]
      + b_dire_elo * dire_elo[i]
    );
  }
}
