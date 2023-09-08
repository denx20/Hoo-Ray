# \begin{table}[h]
# \begin{tabular}{l|ccc}
#                & \multicolumn{3}{c}{Task Size}                                              \\
# Execution time (s) & \multicolumn{1}{l}{50} & \multicolumn{1}{l}{100} & \multicolumn{1}{l}{200} \\ \hline
# Single thread  & 14                     & 32                      & 53                      \\
# 3 workers      & 6                      & 13                      & 30                      \\
# 5 workers      & 6                      & 11                      & 25                      \\
# 10 workers     & 4                      & 9                       & 21                      \\
# 16 workers     & 3                      & 7                       & 19                      \\
# SMP            & 4                      & 8                       & 10                     
# \end{tabular}
# \caption{Benchmark results on large matrix operation tasks}
# \end{table}

# Put the above data in a pandas dataframe

import pandas as pd

df = pd.DataFrame()
df['single thread'] = [14, 32, 53]
df['3 workers'] = [6, 13, 30]
df['5 workers'] = [6, 11, 25]
df['10 workers'] = [4, 9, 21]
df['16 workers'] = [3, 7, 19]
df['SMP (16 cores)'] = [4, 8, 10]
df.index = [50, 100, 200]

