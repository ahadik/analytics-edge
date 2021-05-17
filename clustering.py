'''
  Go through all artwork jsons and extract all subject IDs along with artwork details

  Outputs a CSV of artwork details and subject IDs
'''


import json
import csv
from os import walk
import numpy as np
import unicodedata

global id_details
global artwork_subjects
global artwork_details
global freq_cutoff

id_details = {}
artwork_subjects = {}
artwork_details = {}
freq_cutoff = 0

def open_files(targetpath):
  global id_details
  global artwork_subjects
  global freq_cutoff
  global artwork_details
    
  for dirname, dirnames, filenames in walk(targetpath):
    for filename in filenames:
      filepath = '/'.join([dirname,filename])
      fileopen = open(filepath).read().decode('utf-8')
      jsonopen = json.loads(fileopen)

      subject_ids = get_all_subjects(jsonopen)
      artwork_subjects[jsonopen['id']] = subject_ids
      artwork_details[jsonopen['id']] = {
        'title': jsonopen['title'],
        'artist': jsonopen['all_artists'],
        'url': jsonopen['thumbnailUrl']
      }
        

  with open('artworks.csv', 'wb') as csvfile:
    artwriter = csv.DictWriter(csvfile, fieldnames=['id', 'subjects', 'title', 'artist', 'url'])
    artwriter.writeheader()

    for id, subject_ids in artwork_subjects.items():
      filtered_subject_ids = filter(lambda id: id_details[id]['count'] > freq_cutoff, subject_ids )
      if len(filtered_subject_ids) > 0:
        artwriter.writerow({
          'id': id,
          'subjects': np.array2string(np.unique(filtered_subject_ids), max_line_width = np.inf, separator=",").replace(" ", "")[1:-1],
          'title': artwork_details[id]['title'].encode("ascii", "ignore"),
          'artist': artwork_details[id]['artist'].encode("ascii", "ignore"),
          'url': artwork_details[id]['url']
        })

  with open('id_details_total.csv', 'wb') as csvfile:
    idwriter = csv.DictWriter(csvfile, fieldnames=['id', 'name', 'count'])
    idwriter.writeheader()
    
    for id, details in id_details.items():
      if details['count'] > freq_cutoff:
        idwriter.writerow({ 'id': id, 'name': details['name'].encode("ascii", "ignore"), 'count': details['count'] })



def get_all_subjects(jsonfile):
  subjectcount = jsonfile['subjectCount']
  if subjectcount != 0:
    return extract_subjects(jsonfile['subjects']['children'] if jsonfile.has_key('children') else [jsonfile['subjects']])
  return []
    

def extract_subjects(children, ids=[]):
  global id_details
  for child in children:
    if id_details.has_key(child['id']):
      id_details[child['id']]['count'] += 1
    else:
      id_details[child['id']] = {
        'name': child['name'],
        'count': 1
      }
    if child.has_key('children'):
      ids = extract_subjects(child['children'], ids + [child['id']])
    else:
      ids += [child['id']]
  
  return ids

open_files('./data/artworks')
