## First, lets build the stan model! ##

dota.dat <- list(
  N_matches = nrow(clean_DPC_data),
  radiant_win = clean_DPC_data$radiant_win,
  is_major = clean_DPC_data$league_type_Major,
  is_div_1 = clean_DPC_data$league_type_Div_1,
  is_div_2 = clean_DPC_data$league_type_Div_2,
  is_qual  = clean_DPC_data$league_type_Qual ,
  # First, the radiant statistics
  radiant_team_GPM = clean_DPC_data$radiant_team_GPM,
  radiant_team_XPM = clean_DPC_data$radiant_team_XPM,
  radiant_pos1_KDA = clean_DPC_data$radiant_pos1_KDA,
  radiant_pos1_GPM = clean_DPC_data$radiant_pos1_GPM,
  radiant_pos1_XPM = clean_DPC_data$radiant_pos1_XPM,
  radiant_pos1_LH  = clean_DPC_data$radiant_pos1_LH ,
  radiant_pos5_GPM = clean_DPC_data$radiant_pos5_GPM,
  radiant_pos5_XPM = clean_DPC_data$radiant_pos5_XPM,
  radiant_pos5_KDA = clean_DPC_data$radiant_pos5_KDA,
  radiant_pos5_LH  = clean_DPC_data$radiant_pos5_LH ,
  radiant_LH_range = clean_DPC_data$radiant_LH_range,
  radiant_GPM_range= clean_DPC_data$radiant_GPM_range,
  radiant_XPM_range= clean_DPC_data$radiant_XPM_range,
  radiant_elo = clean_DPC_data$radiant_elo,
  # Next, the dire statistics
  dire_team_GPM = clean_DPC_data$dire_team_GPM,
  dire_team_XPM = clean_DPC_data$dire_team_XPM,
  dire_pos1_KDA = clean_DPC_data$dire_pos1_KDA,
  dire_pos1_GPM = clean_DPC_data$dire_pos1_GPM,
  dire_pos1_XPM = clean_DPC_data$dire_pos1_XPM,
  dire_pos1_LH  = clean_DPC_data$dire_pos1_LH ,
  dire_pos5_GPM = clean_DPC_data$dire_pos5_GPM,
  dire_pos5_XPM = clean_DPC_data$dire_pos5_XPM,
  dire_pos5_KDA = clean_DPC_data$dire_pos5_KDA,
  dire_pos5_LH  = clean_DPC_data$dire_pos5_LH ,
  dire_LH_range = clean_DPC_data$dire_LH_range,
  dire_GPM_range= clean_DPC_data$dire_GPM_range,
  dire_XPM_range= clean_DPC_data$dire_XPM_range,
  dire_elo = clean_DPC_data$dire_elo
)

## Here are the stan models that I tested, and the ones that I was most interested in ##
dota2_self_elo.fit_with_xpm_gpm <- stan(file = 'DPC-Games-With-Self-calculated-Elo.stan', data = dota.dat)
looic_xpm_gpm <- loo(extract_log_lik(dota2_self_elo.fit_with_xpm_gpm))

# I ended up choosing this one
dota2_self_elo.fit_gpm <- stan(file = 'DPC-Games-With-Self-calculated-Elo-GPM-only.stan', data = dota.dat)
looic_GPM <- loo(extract_log_lik(dota2_self_elo.fit_gpm)) # Looks like not including XPM produces a better fit!

dota2_self_elo.fit_xpm <- stan(file = 'DPC-Games-With-Self-calculated-Elo-XPM-only.stan', data = dota.dat)
looic_XPM <- loo(extract_log_lik(dota2_self_elo.fit_xpm))

dota2_self_elo.fit_gpm_pos5gpm <- stan(file = 'DPC-Games-With-Self-calculated-Elo-GPM-pos1KDA.stan', data = dota.dat)
looic_gpm_pos5gpm <- loo(extract_log_lik(dota2_self_elo.fit_gpm_pos5gpm))

dota2_self_elo.fit_gpm_div <- stan(file = 'DPC-Games-With-Self-calculated-Elo-gpm-division-effect.stan', data = dota.dat)
looic_gpm_div <- loo(extract_log_lik(dota2_self_elo.fit_gpm_div))