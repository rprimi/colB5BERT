import os
os.environ['LABAPE6_HOST'] = 'mysql08.labape.com.br'
os.environ['LABAPE6_USER'] = 'labape6'
os.environ['LABAPE6_PASSWORD'] = 'yyzrushk98159'
os.environ['LABAPE6_DBNAME'] = 'labape6'


# Establish the connection
conn = pymysql.connect(
    host = os.getenv('LABAPE6_HOST'),
    user = os.getenv('LABAPE6_USER'),
    password = os.getenv('LABAPE6_PASSWORD'),
    db = os.getenv('LABAPE6_DBNAME')
)


db_config = {
    'user': 'labape6',
    'password': 'yyzrushk98159',
    'host':  'mysql08.labape.com.br',
    'database': 'labape6',
    'raise_on_warnings': True
}
