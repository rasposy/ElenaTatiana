import React, { Component } from 'react';
import axios from 'axios';
import Table from 'react-bootstrap/Table'
import 'bootstrap/dist/css/bootstrap.min.css';

class Primy extends Component {
  state = {
    primes: [],

  };
  constructor(props) {
    super(props);

  }


  componentDidMount() {
    axios.get(`http://localhost:8080/primes`)
      .then(res => {
        const primes = res.data;
        this.setState({ primes: primes });
      })
  }
  render() {
    return (
      <>
        <Table striped bordered hover>
          <thead>
            <tr>
              <th>Prime number </th>
            </tr>
          </thead>
          <tbody>
            {this.state.primy.map(prime => <tr><td>{prime.number}</td></tr>)}
          </tbody>
        </Table>
      </>
    );
  }
}

export default Primy;
