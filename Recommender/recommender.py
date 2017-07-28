#! /bin/python

## ------------------------------
## Libraries
## ------------------------------
import sys
import foursquare


## ------------------------------
## Functions
## ------------------------------
def four_square_recomm(query):
    # Construct the client object
    client = foursquare.Foursquare(client_id='V3A4HR2P1Y2ZE55M4CNOBFTKNAAJXLGO5JJHBVVX0MSJZRVH',
                                   client_secret='DBD2XUS3D2MQIWA1LFVP3MOT3EAWF2RRPSK10E1TNAYSILNF',
                                   redirect_uri='http://localhost:8000')

    # Build the authorization url for your app
    auth_uri = client.oauth.auth_url()

    # Interrogate foursquare's servers to get the user's access_token
    ## access_token = client.oauth.get_token('1WD1HI0BUS4HXMRLXB5BOFIUM35OK4U4JKAIIQDGTTXPDRCK')

    # Apply the returned access token to the client
    client.set_access_token('FAMQ4SAF0ROR4WFGDN2DQFKBBP4FRNRMFXDBBMJVYP2Z50VF')

    # Get the user's data
    user = client.users()

    test = client.venues.search(params={'query': query, 'near': 'Mexico City'})


## ------------------------------
## MAIN
## ------------------------------
if __name__ == '__main__':
    query = sys.stdin.readline()
    print query
