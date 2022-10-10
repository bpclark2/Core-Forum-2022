from pymarc import MARCReader
import csv, json, re

csv_file = open('subjects.csv', 'w', encoding='utf-8')
csv_writer = csv.writer(csv_file, delimiter=',', quotechar='"')
headers = ('BIB_ID', 'PUBYEAR', 'LCC', 'LCSH')
csv_writer.writerow(headers)
with open('core-forum-records.mrc', 'rb') as fh:
   reader = MARCReader(fh)
   for record in reader:
      row = []
      lcsh = []
      bib_id = str(record['001'])[6:]
      row.append(bib_id)
      pubyear = record.pubyear()
      if pubyear:
         try:
            pub_year_only = re.search(r'([1-2][0-9]{3})', pubyear).group()
            row.append(pub_year_only)
         except AttributeError:
            row.append('NULL')
      else:
         row.append('NULL')
      if record['050']:
         lcc = str(record['050'])[6:]
         row.append(lcc)
      else:
         row.append('NULL')
      if record['650']:
         subjects = record.get_fields('650')
         for f in subjects:
            subj = str(f)
            ind_2 = subj[7]
            subj_formatted = subj[8:]
            no_sub_0 = subj_formatted.split('$0')[0]
            if ind_2 == '0':
               lcsh.append(no_sub_0)
         num = len(lcsh)
         if num != 0:
               row.append(json.dumps(lcsh))
         else:
               row.append('NULL')
      else:
         row.append('NULL')
      csv_writer.writerow(row)

csv_file.close()

