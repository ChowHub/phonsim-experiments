"""
Preprocess data for PhonSim project. Should work with Pandas v0.11/.12. Be wary
of using with newer versions.

Michael Chow (machow@princeton.edu)

"""


import pandas as pd
from pandas import DataFrame
from pandas.core.reshape import melt
import re, os

def fill_null(data, col1, col2):
    """Replace null values in col1 with values in col2"""
    data[col1] = data.apply(lambda df: df[col2] if not pd.isnull(df[col2]) else df[col1], axis=1)
    return data

def re_multisub(toremove, x):
    """Strip multiple terms from a string"""
    toremove = [str(ii) for ii in toremove]
    substr = '|'.join(toremove)
    return re.sub(substr, '', x)

def rankinput(df, col):
    """assign df[col] = 0 to nrow df"""
    df[col] = range(df.shape[0])
    return df

def cum_unique(df, col):
    """Assign trialnum col to df. col entries start at 0, then increment for
    each additional unique item.
    """
    x = df[col]
    uniq = set()
    l = []
    for ii in x:
        uniq.add(ii)
        l.append(len(uniq) - 1)
    df['trialnum'] =  l
    return df

#Format TBR items
def procTBR(data, keep, dv='TBR'):
    dist = data[data[dv].notnull()]
    dist = dist[keep]
    dist = dist.groupby(['Subject', 'trialnum']).apply(rankinput, col='inpos')
    return dist

def procRec(data):
    """Format Recall.
    """
    recall = data[data['recall'].notnull()][['Subject', 'trialtype', 'trialnum', 'recall']]
    #concat all resps, to search for unwanted entries in brackets (e.g. {UP})
    brackets = re.findall('{.*?}', reduce(lambda x,y: x + y, recall['recall']).replace('{ENTER}', ''))      
    toremove = list(set(brackets))
    toremove.extend(range(10) + ['\t', " "])    # also remove numbers, and whitespace

    # removes all str in toremove from s
    def clean_resps(s): return re_multisub(toremove, s)

    # Split entries by {ENTER}
    S = recall['recall'].map(clean_resps).map(lambda s: s.split('{ENTER}'))
    # Create df with trial on rows (as in input), and each resp input is a column
    # Make lowercase (while propogating None)
    tmp_df = DataFrame(list(S.values), index=S.index).applymap(lambda s: s.lower() if s is not None else None)                                               #Wide DF with resp# on the columns
    # Merge with original recall trial data
    all_recall = recall.drop('recall', axis=1).merge(tmp_df, left_index=True, right_index=True)
    recall_long = melt(all_recall, id_vars=['Subject', 'trialtype', 'trialnum'])
    recall_long.rename(columns={'variable':'outpos', 'value':'resp'}, inplace=True)
    recall_long = recall_long[ ~recall_long['resp'].isnull() ]
    return recall_long

def procWide(data, value_vars, newcol):
    tbr = melt(data, id_vars = ['Subject', 'trialtype', 'trialnum'], value_vars = value_vars)
    tbr.rename(columns={'variable':'inpos', 'value':newcol}, inplace=True)
    tbr['inpos'] = tbr['inpos'].apply(lambda s: int(s[-1]) -1) #go back to 0-indexing
    tbr = tbr[~tbr[newcol].isnull()]
    tbr =  tbr.sort(['Subject', 'trialnum', 'inpos'])
    return tbr

def make_indx(trialtype='Running[Trial]',
              TBR='words',
              dist_RT='TextDisplay1.RT',
              recall='TextDisplay3.RESP',
              recall_RT='TextDisplay3.RT',
              digits='digits'):
    return dict((v,k) for k,v in locals().iteritems() if v)

def parse(data, indx_map, is_wide=False, tofill=None, **kwargs): #kwargs just takes care of the 'task' entry (TODO less hackish)
    #Load data
    data = pd.read_csv(fname, sep='\t')
    data.rename(columns=indx_map, inplace=True)
    if tofill:
        for col1, col2 in tofill:
            fill_null(data, col1, col2)

    if is_wide:
        #Put trialnum, trialtype, and setsize information into DF
        data = data.groupby(['Subject']).apply(rankinput, col='trialnum')
        data['trialtype'] = data['trialtype'].apply(lambda s: 'P' if pd.isnull(s) else s[0])
        #Get DF with TBR data
        tbr = procWide(data, ['Word%s'%ii for ii in range(1,7)], 'TBR')
        #Get DF with RT data
        oldRT = ['%s.RT'%num for num in ('One', 'Two', 'Three', 'Four', 'Five', 'Six')]         #make replacement RT column labels, so like word columns
        newRT = ['RT.%d'%ii for ii in range(1,7)]
        data.rename(columns = dict(zip(oldRT, newRT)), inplace=True)                            #replace columns
        dist = procWide(data, newRT, 'dist_RT')
    else:
        #Put trialnum information into DF                                       #TODO trial type and set size information
        data = data.groupby('Subject').apply(cum_unique, 'trialtype')
        data['trialtype'] = data['trialtype'].apply(lambda s: s[0])
        keep = ['Subject', 'trialtype', 'trialnum', 'TBR']                       #break Running[Trial] into cond, len, and trialnum
        #Get DF with TBR data
        tbr = procTBR(data, keep, 'TBR')
        #GET DF with RT data
        dist = procTBR(data, ['Subject', 'trialtype', 'trialnum', 'dist_RT'], 'dist_RT')

    recall = procRec(data)
    #Merge the two
    tmp = pd.merge(tbr, recall, how='outer', left_on=['Subject', 'trialtype', 'trialnum', 'inpos'], right_on=['Subject', 'trialtype', 'trialnum', 'outpos'])
    has_rt = True
    if has_rt:
        final = pd.merge(tmp, dist, how='outer', left_on=['Subject', 'trialtype', 'trialnum', 'inpos'], right_on=['Subject', 'trialtype', 'trialnum', 'inpos'])
    return final

#RUN SCRIPT
####
tofill = [['recall', 'Recall.RESP[SubTrial]'],
          ['Word1', 'PracWord1'],
          ['Word2', 'PracWord2']]
####

wide_indx = make_indx(trialtype='Phonological', recall='Recall.RESP[Trial]')

# get header for E10
E10_indx = {entry: entry[2:] for entry in pd.read_csv('Data/raw/E10oscram/raw_data.txt', sep='\t').columns if entry.startswith('OS')}
E10_indx.update(make_indx(trialtype='Phonological', recall='OSRecall.RESP[SubTrial]'))
print E10_indx

# Arguments to parsing function
Exp =  {'Data/raw/E9pool/raw_data.txt':              dict(task = 'R.pool350',    indx_map = make_indx()),
        'Data/raw/E9poolb/raw_data.txt':             dict(task = 'R.poolb',      indx_map = make_indx()),
        'Data/raw/E11names/raw_data_longnames.txt':  dict(task = 'Rspan.names.long',  indx_map = make_indx()),
        'Data/raw/E11names/raw_data_shortnames.txt': dict(task = 'Rspan.names.short', indx_map = make_indx()),

        #Wide tasks
        'Data/raw/E2/raw_data_RTshort.txt':         dict(task = 'Rspan.short',  indx_map = wide_indx, is_wide = True,
                                                         tofill = [['recall', 'Recall.RESP[Block]'], ['Word1', 'PracWord1'], ['Word2', 'PracWord2']]),
        'Data/raw/E2/raw_data_RTlong.txt':          dict(task = 'Rspan.long',   indx_map = wide_indx, is_wide = True,
                                                         tofill = [['recall', 'Recall.RESP[Block]'], ['Word1', 'PracWord1'], ['Word2', 'PracWord2']]),
        'Data/raw/E4/raw_data.txt':                  dict(task = 'Ospan.reg',    indx_map = wide_indx, is_wide = True,
                                                         tofill = [['recall', 'Recall.RESP[Block]'], ['Word1', 'PracWord1'], ['Word2', 'PracWord2']]),
        'Data/raw/E7/raw_data_RS.txt':                dict(task = 'R.scramb',     indx_map = wide_indx, is_wide = True, tofill = tofill),
        'Data/raw/E7/raw_data_OS.txt':                dict(task = 'spOspan.noVer', indx_map = wide_indx, is_wide = True, tofill = tofill),
        'Data/raw/E10oscram/raw_data.txt':           dict(task = 'Ospan.scram.noVer', indx_map = E10_indx, is_wide = True,
                                                         tofill = [['recall', 'Recall.RESP[Trial]'], ['Word1', 'PracWord1'], ['Word2', 'PracWord2']])}

out = {}
all_df = []
for fname in Exp:
    try: os.mkdir('Data/0_preprocess')
    except OSError: pass
    print fname
    folder = fname.split('/')[-2]
    kwargs = Exp[fname]
    data = parse(fname, **kwargs)
    data['task'] = kwargs['task']
    data['folder'] = folder
    base, ext = os.path.splitext(os.path.basename(fname))
    data.to_csv('Data/0_preprocess/' + base + '_long.csv')
    all_df.append(data)
big_df = pd.concat(all_df)
big_df.to_csv('Data/scored_all.csv')
