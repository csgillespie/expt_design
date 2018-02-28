perturb_times = function(times) {
  k = if(length(times) == 2) 2 else sample(2:length(times), 1)
  old_time = times[k]
  times[k] = times[k] + rpois(1, 2) - rpois(1, 2)
  if(times[k] < 0 || times[k] > 50) times[k] = old_time
  if(any(duplicated(times))) times[k] = old_time
  sort(times)
}
