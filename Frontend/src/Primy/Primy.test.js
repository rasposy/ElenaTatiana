import React from "react";
import { shallow } from "enzyme";
import Primy from "./Primy";

describe("Bookstore", () => {
  test("matches snapshot", () => {
    const wrapper = shallow(<Primy />);
    expect(wrapper).toMatchSnapshot();
  });
});
