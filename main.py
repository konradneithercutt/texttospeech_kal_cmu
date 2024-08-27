import os,re
import numpy as np
from scipy.io import wavfile
import matplotlib.pyplot as plt
from IPython.display import Audio

kal = "./cmu_us_kal_diphone/"

assert len(os.listdir(kal)) == 21

def getbit(start,end,filename,dirname=kal):
    sr,w = wavfile.read(dirname+'wav/'+filename)
    startidx = int(sr * start)
    endidx = int(sr*end)
    return w[startidx:endidx]

def makeDiphones(where):
    diphones = {}
    for i in range(1,1350):
        filename = str(i)
        while len(filename) < 4: filename = '0' + filename
        filename = 'kal_' + filename + '.lab'
        f = open(kal+'lab/'+filename,'r')
        t = f.read()
        f.close()
        t = re.sub(' 26 ','',t)
        t = t.split('\n')
        t = t[3:-1]
        fset = []
        start = 0
        for line in t:
            bits = line.split('\t')
            bits = bits[1:]
            fset.append((float(start),float(bits[0]),bits[1]))
            start = bits[0]
        i = 1
        while i < len(fset):
            diphone = fset[i-1][2] + '-' + fset[i][2]
            start = (fset[i-1][1] - fset[i-1][0])/2 + fset[i-1][0]
            end = (fset[i][1] - fset[i][0])/2 + fset[i][0]
            filename = re.sub('\.lab$','.wav',filename)
            triple = (filename,start,end)
            if diphone in diphones:
                diphones[diphone].append(triple)
            else:
                diphones[diphone] = [triple]
            i += 1
    return diphones

diphones = makeDiphones(kal)

cmufile = "./cmudict-0.7b.txt"

f = open(cmufile,'rb')
t = f.read()
f.close()
cmudict = t.decode('ISO-8859-1')
assert np.isclose(len(cmudict),3865710,atol=2)


def makeDictionary(datafile):
    '''map the cmudict text file to a dictionary
    args:
        datafile: the cmudict07b file as a list of lines
    returns:
        a dictionary from spellings to a list of lists of
        characters.
    '''
    datafile = datafile.lower()
    ls = datafile.split("\r\n")
    ls = ls[126:-6]

    d = {}
    
    for line in ls:
        firstspl = line.split("  ")
        key = firstspl[0]
        if key[-1] == ')':
            key = key[:-3]
        oldvalue = firstspl[1].split(" ")
        
        value = []
        for elem in oldvalue:
            if elem == '':
                continue
            if elem[-1] in "0123456789":
                value.append(elem[:-1])
            else:
                value.append(elem)
        
        if key in d:
            temp = d[key]
            temp.append(value)
            d[key] = temp
        else:
            try:
                d[key] = [value]
            except:
                continue
        for i in range(125696,125770):
            d[i] = "ah"
                
    return d

d = makeDictionary(cmudict)
assert len(d) == 125770

assert d['tomato'] == [
    ['t', 'ah', 'm', 'ey', 't', 'ow'],
    ['t', 'ah', 'm', 'aa', 't', 'ow']
]

assert d["didn't"] == [
    ['d', 'ih', 'd', 'ah', 'n', 't'],
    ['d', 'ih', 'd', 'n', 't'],
    ['d', 'ih', 'd', 'ah', 'n'],
    ['d', 'ih', 'n', 't']
]

def sentence2diphones(s,c):
    '''converts a sentence into a sequence of diphones
    args:
        s: a sentence as a string
        c: cmu dictionary as a dictionary
    returns:
        a list of diphones with 'pau' at the beginning
        and end, and between words
    '''
    res = []
    s = s[:-1]
    s = s.lower()
    wordbyword = s.split()
    d = makeDictionary(cmudict)
    for word in wordbyword:
        if word in d:
            phones = d[word]
        else:
            phones = [['ah']]
        for phone in phones[0]:
            if res == [] or res[-1].endswith("pau"):
                res.append("pau-"+phone)
            else:
                res.append(res[-1][res[-1].index("-")+1:] + "-" + phone)
        res.append(phones[0][-1]+"-pau")
    return res

words = "I'm feeling pretty optimistic right now."
res = sentence2diphones(words,d)
assert len(res) == 33

assert res == [
    'pau-ay','ay-m','m-pau','pau-f','f-iy',
    'iy-l','l-ih','ih-ng','ng-pau','pau-p',
    'p-r','r-ih','ih-t','t-iy','iy-pau',
    'pau-aa','aa-p','p-t','t-ih','ih-m',
    'm-ih','ih-s','s-t','t-ih','ih-k',
    'k-pau','pau-r','r-ay','ay-t','t-pau',
    'pau-n','n-aw','aw-pau'
]

words = "The bznork sings!"
res = sentence2diphones(words,d)
assert res == [
    'pau-dh','dh-ah','ah-pau','pau-ah','ah-pau',
    'pau-s','s-ih','ih-ng','ng-z','z-pau'
]

def makeWave(s,dps):
    '''converts a sequence of diphones into a wave
    args:
        s: a sequence of diphones
        dps: diphone database
    returns:
        a wave as a numpy array
    '''
    res = []
    for diphone in s:
        triple = diphones[diphone][0]
        w = getbit(triple[1],triple[2],triple[0])
        res.append(w)
    return np.hstack(res)

s = "I like pancakes."
res = sentence2diphones(s,d)
w = makeWave(res,diphones)
assert type(w) == np.ndarray

assert np.isclose(len(w),58119,atol=50)

plt.plot(w)
plt.show()

wavfile.write("output.wav", 16000, w)