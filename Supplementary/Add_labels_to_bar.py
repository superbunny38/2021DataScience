# importing library
import matplotlib.pyplot as plt
 
# function to add value labels
def addlabels(x,y):
    for i in range(len(x)):
        plt.text(i, y[i]+30, y[i], ha = 'center')
