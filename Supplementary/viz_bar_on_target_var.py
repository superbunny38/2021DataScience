# Visualizing target variable by bar plots

import matplotlib.pyplot as plt
 
# function to add value labels
def addlabels(x,y):
    for i in range(len(x)):
        plt.text(i, y[i]+30, y[i], ha = 'center')
        
import seaborn as sns
import matplotlib.pyplot as plt
x = list(Counter(train_df['type']).keys())
y = list(Counter(train_df['type']).values())
plt.bar(x,y)
plt.xticks([0,1,2,3])
addlabels(x,y)
plt.ylabel("number of data")
plt.title("Target variable distribution")
plt.xlabel("Type")
plt.show()
